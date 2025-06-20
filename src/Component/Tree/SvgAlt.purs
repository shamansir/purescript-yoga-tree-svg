module Yoga.Tree.Svg.Component.Tree.SvgAlt where

import Prelude

import Debug as Debug


import Data.Maybe (Maybe(..))
import Data.Graph (Graph)
import Data.Graph as Graph
import Data.Map (Map)
import Data.Map as Map
import Data.Array as Array
import Data.Tuple as Tuple
import Data.Int as Int
import Data.List (List(..), (:))
import Data.Foldable (class Foldable, foldl)
import Data.FunctorWithIndex (mapWithIndex)
import Data.Tuple.Nested ((/\), type (/\))

import Yoga.Tree (Tree)
import Yoga.Tree as Tree
import Yoga.Tree.Extended as Tree
import Yoga.Tree.Extended.Path (Path)
import Yoga.Tree.Extended.Path as Path


import Halogen as H
import Halogen.HTML (HTML)
import Halogen.HTML as HH
import Halogen.HTML.Properties as HHP
import Halogen.HTML.Events as HE
import Halogen.Svg.Attributes as HSA
import Halogen.Svg.Attributes.FontSize (FontSize(..)) as HSA
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


foldl' :: forall f a b. Foldable f => (b -> a -> b) -> f a -> b -> b
foldl' = flip <<< foldl


positions :: forall a. Graph Path a -> Graph Path { x :: Number, y :: Number, value :: a }
positions graph =
    graphMap
        # mapWithIndex ((/\))
        # foldl distribute Map.empty
        # Graph.fromMap

    where
        graphMap = Graph.toMap graph
        zeroPos = { x : 0.0, y : 0.0 }
        childPos parentPos childIndex =
            { x : parentPos.x + 5.0, y : parentPos.y + 15.0 + (Int.toNumber childIndex * 15.0) }
        mergeWithValue value pos =
            { x : pos.x, y : pos.y, value }

        distribute prevPositions (curPath /\ curValue /\ childPaths) =
            case prevPositions # Map.lookup curPath <#> Tuple.fst of
                Just { x, y } ->
                    prevPositions
                        # (foldl' (insertChildPos { x, y }) $ mapWithIndex (/\) childPaths)
                Nothing ->
                    prevPositions
                        # Map.insert curPath (mergeWithValue curValue zeroPos /\ childPaths)
                        # (foldl' (insertChildPos zeroPos) $ mapWithIndex (/\) childPaths)

        insertChildPos parentPos posMap (index /\ childPath) =
            case graphMap # Map.lookup childPath of
                Just (value /\ childPaths) ->
                    posMap
                        # Map.insert childPath ((mergeWithValue value $ childPos parentPos index) /\ childPaths)
                Nothing ->
                    posMap



renderGraph :: forall a p i. Show a => Graph Path a -> Array (HTML p i)
renderGraph graph = foldl (<>) [] $ renderNode <$> positionsMap
    where
        positionsMap = Graph.toMap $ positions graph
        renderValue label { x, y, value } =
            HS.text
                [ HSA.fill $ HSA.RGB 0 0 0
                , HSA.x x
                , HSA.y y
                ]
                [ HH.text $ label <> show value ]
        renderChild path =
            case Map.lookup path positionsMap <#> Tuple.fst of
                Just val -> renderValue "child" val
                Nothing -> HS.g [] []
        renderNode (a /\ paths) =
                [ renderValue "node" a
                ] <> (renderChild <$> Array.fromFoldable paths)
