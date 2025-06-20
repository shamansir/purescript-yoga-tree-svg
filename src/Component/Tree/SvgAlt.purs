module Yoga.Tree.Svg.Component.Tree.SvgAlt where

import Prelude

import Data.Number (pi, cos, sin) as Number
import Data.Maybe (Maybe(..), fromMaybe)
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
type PositionedMap a = Map Path (Positioned a)
type PositionedGraphMap a = Map Path (Positioned a /\ List Path)


positions :: forall a. Graph Path a -> Graph Path (Positioned a)
positions graph =
    graphMap
        # mapWithIndex ((/\))
        # foldl (flip distribute) Map.empty
        # fillBackChildren
        # Graph.fromMap

    where
        graphMap = Graph.toMap graph
        zeroPos = { x : 0.0, y : 0.0 }
        childPos parentPos =
            findPosition parentPos 0.0 (2.0 * Number.pi / 3.0)
        mergeWithValue value pos =
            { x : pos.x, y : pos.y, value }

        fillBackChildren :: PositionedMap a -> PositionedGraphMap a
        fillBackChildren = mapWithIndex \path cell -> cell /\ (Map.lookup path graphMap <#> Tuple.snd # fromMaybe Nil)

        distribute :: Path /\ a /\ List Path -> PositionedMap a -> PositionedMap a
        distribute (curPath /\ curValue /\ childPaths) prevPositions =
            case prevPositions # Map.lookup curPath of
                Just { x, y } ->
                    prevPositions
                        # positionChildrenFrom { x, y } childPaths
                Nothing ->
                    prevPositions
                        # storePosition curPath curValue zeroPos
                        # positionChildrenFrom zeroPos childPaths

        storePosition :: Path -> a -> Position -> PositionedMap a -> PositionedMap a
        storePosition path value position =
            Map.insert path $ mergeWithValue value position

        positionChildrenFrom :: Position -> List Path -> PositionedMap a -> PositionedMap a
        positionChildrenFrom parentPos childPaths =
            foldl' (flip $ insertChildPos parentPos $ List.length childPaths) $ mapWithIndex (/\) (List.reverse childPaths)

        insertChildPos :: Position -> Int -> Int /\ Path -> PositionedMap a -> PositionedMap a
        insertChildPos parentPos childrenCount (index /\ childPath) prevPositions =
            case graphMap # Map.lookup childPath <#> Tuple.fst of
                Just value ->
                    prevPositions
                        # storePosition childPath value (childPos parentPos childrenCount index)
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
                    , HSA.x 6.0
                    , HSA.y 9.0
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
