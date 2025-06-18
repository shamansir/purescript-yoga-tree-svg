module Yoga.Tree.Svg.Component.Tree.Lib where

import Prelude


foo = 42


{-| -}
{-
preOrder : Tree a -> List a
preOrder tree =
    loop (init tree) nextStep
        |> List.reverse
-}


{-| -}
{-
levelOrder : Tree a -> List a
levelOrder tree =
    lox [ tree ]
-}


{-|

    Return a list of pairs (x,y) where

        - x and y are of type a
        - (x,y) is in the list if and only if y is a chiild of x.

-}
{-
edges : Tree a -> List ( a, a )
edges tree =
    edgesAux { trees = [ tree ], edges = [] } |> .edges |> List.reverse
-}


{-
type alias EdgeState a =
    { trees : List (Tree a), edges : List ( a, a ) }
-}


{-
edgesAux : EdgeState a -> EdgeState a
edgesAux state =
    case List.head state.trees of
        Nothing ->
            state

        Just tree ->
            let
                children =
                    Tree.children tree

                tos =
                    List.map Tree.label children

                from =
                    Tree.label tree

                newEdges =
                    List.map (\to -> ( from, to )) tos |> List.reverse
            in
            edgesAux
                { trees = children ++ List.drop 1 state.trees
                , edges = newEdges ++ state.edges
                }
-}



--FOR PREORDER


{-
type alias State a =
    { zipper : Maybe (Zipper a)
    , nodes : List a
    }
-}


{-
init : Tree a -> State a
init tree =
    let
        initialZipper =
            Zipper.fromTree tree

        firstNode =
            Zipper.label initialZipper
    in
    { zipper = Just initialZipper
    , nodes = [ firstNode ]
    }
-}


{-
nextStep : State a -> Step (State a) (List a)
nextStep state =
    case state.zipper of
        Nothing ->
            Done state.nodes

        Just z ->
            let
                maybeNewZipper =
                    Zipper.forward z
            in
            case maybeNewZipper of
                Nothing ->
                    Done state.nodes

                Just newZipper ->
                    Loop { zipper = Just newZipper, nodes = Zipper.label newZipper :: state.nodes }
-}



-- FOR EDGES


{-
type alias StateF a b =
    { zipper : Maybe (Zipper a)
    , nodes : List b
    }
-}


{-
repeatF : List ( a, Int ) -> List a
repeatF list =
    List.foldl (\( a, n ) acc -> List.repeat n a ++ acc) [] list |> List.reverse
-}


{-
preorderF : (a -> Tree a -> b) -> Tree a -> List b
preorderF f tree =
    loop (initF f tree) (nextStepF f)
        |> List.reverse
-}


{-
initF : (a -> Tree a -> b) -> Tree a -> StateF a b
initF f tree =
    let
        initialZipper =
            Zipper.fromTree tree

        firstNode =
            Zipper.label initialZipper
    in
    { zipper = Just initialZipper
    , nodes = [ f firstNode tree ]
    }
-}


{-
nextStepF : (a -> Tree a -> b) -> StateF a b -> Step (StateF a b) (List b)
nextStepF f state =
    case state.zipper of
        Nothing ->
            Done state.nodes

        Just z ->
            let
                maybeNewZipper =
                    Zipper.forward z
            in
            case maybeNewZipper of
                Nothing ->
                    Done state.nodes

                Just newZipper ->
                    let
                        currentTree =
                            Zipper.tree newZipper

                        newNode =
                            f (Zipper.label newZipper) currentTree
                    in
                    Loop { zipper = Just newZipper, nodes = newNode :: state.nodes }
-}



-- FOR LEVEL ORDER


{-
lox : List (Tree a) -> List a
lox list =
    case list of
        [] ->
            []

        first :: rest ->
            Tree.label first :: lox (rest ++ Tree.children first)
-}