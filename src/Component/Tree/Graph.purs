module Yoga.Tree.Svg.Component.Tree.Graph where

import Prelude


-- translated from: https://github.com/jxxcarlson/elm-tree-builder/blob/7.0.0/src/Tree/Graph.elm


type Graph e a = Array (Edge e a)


type Edge e a =
    { from :: Node a, to :: Node a, edge :: e }


type Node a =
    { val :: a, x :: Number, y :: Number, r :: Number }