module Yoga.Tree.Svg.Component.Tree.SvgAlt where

import Prelude

import Data.Number (pi, cos, sin) as Number
import Data.Maybe (Maybe(..))
import Data.Graph (Graph)
import Data.Graph (toMap, fromMap) as Graph
import Data.Map (Map)
import Data.Map (empty, lookup, insert) as Map
import Data.Array (fromFoldable, toUnfoldable) as Array
import Data.Tuple (fst, snd) as Tuple
import Data.Int (toNumber) as Int
import Data.List (List(..), (:))
import Data.List (length, reverse) as List
import Data.Foldable (class Foldable, foldl)
import Data.FunctorWithIndex (mapWithIndex)
import Data.Tuple.Nested ((/\), type (/\))

import Yoga.Tree (Tree)
import Yoga.Tree.Extended as Tree
import Yoga.Tree.Extended.Path (Path)
import Yoga.Tree.Extended.Path as Path


import Halogen as H
import Halogen.HTML (HTML)
import Halogen.HTML as HH
import Halogen.HTML.Properties as HHP
-- import Halogen.HTML.Events as HE
import Halogen.Svg.Attributes as HSA
import Halogen.Svg.Elements as HS


toGraph :: forall a. Tree a -> Graph Path a
toGraph = Path.fill >>> Tree.break breakRoot >>> Graph.fromMap
    where
        breakNode theMap (path /\ a) xs =
            theMap
                # Map.insert path (a /\ (Array.toUnfoldable $ Tuple.fst <$> Tree.value <$> xs))
                # foldl' (Tree.break <<< breakNode) xs
        breakRoot =
            breakNode Map.empty


findPosition :: Position -> Number -> Number -> Int -> Int -> Position
findPosition parent startAngle endAngle count index =
    let
        radius = 100.0
        currentAngle = startAngle + ((endAngle - startAngle) / Int.toNumber count) * Int.toNumber index

        co = Number.cos currentAngle
        si = Number.sin currentAngle

        x = parent.x + (co * radius - si * radius)
        y = parent.y + (si * radius + co * radius)
    in
    { x, y }



foldl' :: forall f a b. Foldable f => (b -> a -> b) -> f a -> b -> b
foldl' = flip <<< foldl


type Position = { x :: Number, y :: Number }
type Positioned a = { x :: Number, y :: Number, value :: a }
type PositionedGraphMap a = Map Path (Positioned a /\ List Path)


positions :: forall a. Graph Path a -> Graph Path (Positioned a)
positions graph =
    graphMap
        # mapWithIndex ((/\))
        # foldl (flip distribute) Map.empty
        # Graph.fromMap

    where
        graphMap = Graph.toMap graph
        zeroPos = { x : 0.0, y : 0.0 }
        childPos parentPos childrenCount childIndex =
            findPosition parentPos 0.0 (2.0 * Number.pi / 3.0) childrenCount childIndex
        mergeWithValue value pos =
            { x : pos.x, y : pos.y, value }

        distribute :: Path /\ a /\ List Path -> PositionedGraphMap a -> PositionedGraphMap a
        distribute (curPath /\ curValue /\ childPaths) prevPositions =
            case prevPositions # Map.lookup curPath <#> Tuple.fst of
                Just { x, y } ->
                    prevPositions
                        # positionChildrenFrom { x, y } childPaths
                Nothing ->
                    prevPositions
                        # storePosition curPath curValue zeroPos childPaths
                        # positionChildrenFrom zeroPos childPaths

        storePosition :: Path -> a -> Position -> List Path -> PositionedGraphMap a -> PositionedGraphMap a
        storePosition path value position childPaths =
            Map.insert path $ mergeWithValue value position /\ childPaths

        positionChildrenFrom :: Position -> List Path -> PositionedGraphMap a -> PositionedGraphMap a
        positionChildrenFrom parentPos childPaths =
            foldl' (flip $ insertChildPos parentPos $ List.length childPaths) $ mapWithIndex (/\) (List.reverse childPaths)

        insertChildPos :: Position -> Int -> Int /\ Path -> PositionedGraphMap a -> PositionedGraphMap a
        insertChildPos parentPos childrenCount (index /\ childPath) prevPositions =
            case graphMap # Map.lookup childPath of
                Just (value /\ childPaths) ->
                    prevPositions
                        # storePosition childPath value (childPos parentPos childrenCount index) childPaths
                Nothing ->
                    prevPositions



renderGraph :: forall a p i. Show a => Graph Path a -> Array (HTML p i)
renderGraph graph = foldl (<>) [] $ renderNode <$> positionsMap
    where
        positionsMap :: PositionedGraphMap a
        positionsMap = Graph.toMap $ positions graph
        renderValue label { x, y, value } =
            HS.g
                [ HSA.transform $ pure $ HSA.Translate x y ]
                [ HS.circle
                    [ HSA.cx 0.0
                    , HSA.cy 0.0
                    , HSA.r 5.0
                    , HSA.fill $ HSA.RGB 200 200 200
                    , HSA.stroke $ HSA.RGB 0 0 0
                    ]
                , HS.text
                    [ HSA.fill $ HSA.RGB 0 0 0
                    ]
                    [ HH.text $ label <> show value ]
                ]
        renderChild parent path =
            case Map.lookup path positionsMap <#> Tuple.fst of
                Just child ->
                    HS.line
                        [ HSA.x1 parent.x
                        , HSA.y1 parent.y
                        , HSA.x2 child.x
                        , HSA.y2 child.y
                        , HSA.stroke $ HSA.RGB 0 0 0
                        ]
                Nothing -> HS.g [] []
        renderNode (a /\ paths) =
                [ renderValue "node" a
                ] <> (renderChild a <$> Array.fromFoldable paths)
