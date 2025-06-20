module Yoga.Tree.Svg.Component.Tree.SvgAlt where

import Prelude

import Debug as Debug


import Data.Maybe (Maybe(..))
import Data.Graph (Graph)
import Data.Graph as Graph
import Data.Map as Map
import Data.Array as Array
import Data.Tuple as Tuple
import Data.List (List(..), (:))
import Data.Foldable (foldl)
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
                # \theMap' -> foldl (Tree.break <<< breakNode) theMap' xs
        breakRoot =
            breakNode Map.empty



renderGraph :: forall a p i. Show a => Graph Path a -> Array (HTML p i)
renderGraph graph = foldl appendNode [] graphMap
    where
        graphMap = Graph.toMap graph
        renderNode label a = HS.text [ HSA.fill $ HSA.RGB 0 0 0 ] [ HH.text $ label <> show a ]
        renderChild path =
            case Map.lookup path graphMap <#> Tuple.fst of
                Just val -> renderNode "child" val
                Nothing -> HS.g [] []
        appendNode prev (a /\ paths) =
            prev <>
                [ renderNode "node" a
                ] <> (renderChild <$> Array.fromFoldable paths)
