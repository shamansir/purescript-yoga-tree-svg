module Yoga.Tree.Svg.Component.Tree.Render where

import Prelude

import Control.Monad.Rec.Class (Step(..), tailRec)

import Data.Number (pi, cos, sin)
import Data.Int (toNumber) as Int
import Data.Maybe (Maybe(..))
import Data.Map (Map)
import Data.Map (insert, lookup, empty, fromFoldable, toUnfoldable) as Map
import Data.Tuple (fst, snd, swap) as Tuple
import Data.Tuple.Nested ((/\), type (/\))
import Data.Array ((:))
import Data.Array (head, length, groupBy, drop, reverse) as Array
import Data.Array.NonEmpty (head, toUnfoldable) as NEA
import Data.FunctorWithIndex (mapWithIndex)
import Data.Bifunctor (bimap)
import Data.Foldable (foldl)

import Yoga.Tree (Tree)
import Yoga.Tree.Extended as Tree
import Yoga.Tree.Extended.Path (Path(..))
import Yoga.Tree.Extended.Path as Path
import Yoga.Tree.Svg.Component.Tree.Graph (Graph, Edge, Node)


-- translated from: https://github.com/jxxcarlson/elm-tree-builder/blob/7.0.0/src/Tree/Render.elm


{-| Preferences for function toGraph.
-}
type Preferences =
    { halfAngle :: Number
    , initialEdgeLength :: Number
    , scaleFactor :: Number
    , ballRadius :: Number
    }


{-| Default Preferences
-}
defaults :: Preferences
defaults =
    { halfAngle : 0.25 * pi
    , initialEdgeLength : 2.0
    , scaleFactor : 0.8
    , ballRadius : 0.0
    }


{-| Transform a Tree to a Graph: a data structure that can be rendered to SVG.
-}
-- toGraph : Preferences -> (a -> String) -> Tree a -> Graph
-- toGraph preferences labelToString tree =
--     loop (init preferences labelToString tree) nextStep


type State e a =
    { edgeMap :: Map a (Edge e a)
    , depths :: Map a Int
    , groups :: Array ( ( a /\ a ) /\ Array ( a /\ a ) )
    , graph :: Graph e a
    , preferences :: Preferences
    }


init :: forall x a. Ord a => Preferences -> (x -> a) -> Tree x -> State Unit a
init preferences renderLabel tree =
    let
        depths :: Map a Int
        depths =
            tree # map renderLabel # Path.fillDepths # Tree.flatten <#> Tuple.swap # Map.fromFoldable

        edges :: Array ( a /\ a )
        edges =
            Tree.edges tree <#> bimap renderLabel renderLabel

        edgeGroups :: Array ( ( a /\ a ) /\ Array ( a /\ a ) )
        edgeGroups =
            Array.groupBy (\a b -> Tuple.fst a == Tuple.fst b) edges
                <#> (\nearr -> NEA.head nearr /\ NEA.toUnfoldable nearr)

        root :: Node a
        root =
            { val : Tree.value tree # renderLabel
            , x : 0.0, y : 0.0
            , r : preferences.ballRadius
            }

        origin :: Node a
        origin =
            { val : root.val
            , x : 0.0, y : -preferences.initialEdgeLength
            , r : preferences.ballRadius
            }
    in
    { edgeMap : Map.fromFoldable
                    [ root.val
                    /\
                        { from : origin
                        , to : root
                        , edge : unit
                        }
                    ]
    , depths
    , groups : edgeGroups
    , graph : []
    , preferences
    }


nextStep :: forall e a. Ord a => State e a -> Step (State e a) (Graph e a)
nextStep state =
    case Array.head state.groups of
        Nothing ->
            Done state.graph

        Just (( a /\ b ) /\ rest) ->
            case Map.lookup a state.edgeMap of
                Nothing ->
                    Done state.graph

                Just edge ->
                    let
                        valuePairs :: Array ( a /\ a )
                        valuePairs =
                            ( a /\ b ) : rest

                        newEdges :: Array (Edge e a)
                        newEdges =
                            groupToEdges state.preferences edge valuePairs

                        newMap :: Map a (Edge e a)
                        newMap =
                            foldl (\runningMap e -> runningMap # Map.insert e.to.val e) state.edgeMap newEdges

                        {-
                        info :: Array (a /\ a /\ a)
                        info =
                            newMap # Map.toUnfoldable <#> (\( value /\ e ) -> ( value /\ e.from.val /\ e.to.val ))
                        -}
                    in
                    Loop
                        state
                            { edgeMap = newMap
                            , groups = Array.drop 1 state.groups
                            , graph = state.graph <> newEdges
                            }



theta halfAngle i n =
    if n < 2 then
        0.0

    else
        -halfAngle + (2.0 * Int.toNumber i * halfAngle) / (Int.toNumber n - 1.0)


type Vector =
    { x :: Number, y :: Number }



edgeToVector :: forall e a. Edge e a -> Vector
edgeToVector edge =
    { x : edge.to.x - edge.from.x, y : edge.to.y - edge.from.y }


displaceEdge :: forall e a. Vector -> Edge e a -> Edge e a
displaceEdge vector edge =
    edge { from = displaceNode vector edge.from, to = displaceNode vector edge.to }


displaceNode :: forall a. Vector -> Node a -> Node a
displaceNode vector node =
    node { x = node.x + vector.x, y = node.y + vector.y }


rotateAndRescaleEdge :: forall e a. Number -> Number -> Edge e a -> Edge e a
rotateAndRescaleEdge scaleFacgtor theta_ edge =
    let
        dx =
            edge.to.x - edge.from.x

        dy =
            edge.to.y - edge.from.y

        co =
            cos theta_

        si =
            sin theta_

        xx =
            edge.from.x + scaleFacgtor * (co * dx - si * dy)

        yy =
            edge.from.y + scaleFacgtor * (si * dx + co * dy)

        to =
            edge.to
    in
    edge { to = to { x = xx, y = yy } }



groupToEdges :: forall e a. Preferences -> Edge e a -> Array ( a /\ a ) -> Array (Edge e a)
groupToEdges preferences rootEdge edgesAsLabelPairs =
    let
        n =
            Array.length edgesAsLabelPairs

        dr =
            edgeToVector rootEdge

        start =
            displaceEdge dr rootEdge

        endPoints =
            edgesAsLabelPairs <#> Tuple.snd # Array.reverse

        newEdges =
            mapWithIndex (\i na -> satellite preferences i n na start) endPoints
    in
    newEdges



satellite :: forall e a. Preferences -> Int -> Int -> a -> Edge e a -> Edge e a
satellite preferences i n value edge =
    edge
        # rotateAndRescaleEdge preferences.scaleFactor (theta preferences.halfAngle i n)
        # reassignEndPoint value



reassignEndPoint :: forall e a. a -> Edge e a -> Edge e a
reassignEndPoint value edge =
    let
        to =
            edge.to
    in
    edge { to = to { val = value } }