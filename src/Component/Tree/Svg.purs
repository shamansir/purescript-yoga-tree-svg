module Yoga.Tree.Svg.Component.Tree.Svg where

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
import Data.Bifunctor (lmap)

import Yoga.Tree (Tree)
import Yoga.Tree.Extended as Tree
import Yoga.Tree.Extended.Path (Path(..))
import Yoga.Tree.Extended.Path as Path


import Halogen as H
import Halogen.HTML (HTML)
import Halogen.HTML as HH
import Halogen.HTML.Properties as HHP
import Halogen.HTML.Events as HE
import Halogen.Svg.Attributes as HSA
import Halogen.Svg.Elements as HS


toGraph :: forall a. Tree a -> Graph Path a
toGraph = toGraph' Path.root


toGraph' :: forall a. Path -> Tree a -> Graph Path a
toGraph' (Path root) = Path.fill >>> Tree.break breakRoot >>> Graph.fromMap
    where
        breakNode theMap (path /\ a) xs =
            theMap
                # Map.insert
                    (alignWithRoot path)
                    (a /\
                        (Array.toUnfoldable
                             $  alignWithRoot
                            <$> Tuple.fst
                            <$> Tree.value
                            <$> xs
                        )
                    )
                # foldl' (Tree.break <<< breakNode) xs
        breakRoot =
            breakNode Map.empty
        alignWithRoot (Path path) =
            Path $ root <> path


findPosition :: Number -> Position -> Number -> Number -> Int -> Int -> Position
findPosition baseRadius parent startAngle endAngle count index =
    let
        currentAngle = startAngle + ((endAngle - startAngle) / Int.toNumber count) * Int.toNumber index

        co = Number.cos currentAngle
        si = Number.sin currentAngle

        x = parent.x + (co * baseRadius - si * baseRadius)
        y = parent.y + (si * baseRadius + co * baseRadius)
    in
    { x, y }


rotateBy :: Position -> Position -> Number -> Number -> Position
rotateBy parent current scaleFactor theta =
    let
        dx = current.x - parent.x
        dy = current.y - parent.y
        co = Number.cos theta
        si = Number.sin theta
        xx = parent.x + scaleFactor * (co * dx - si * dy)
        yy = parent.y + scaleFactor * (si * dx + co * dy)
    in
    { x : xx, y : yy }




foldl' :: forall f a b. Foldable f => (b -> a -> b) -> f a -> b -> b
foldl' = flip <<< foldl


type Position = { x :: Number, y :: Number }
type Size = { width :: Number, height :: Number }
type Positioned a = { x :: Number, y :: Number, width :: Number, height :: Number, value :: a }
type PositionedMap a = Map Path (Positioned a)
type PositionedGraphMap a = Map Path (Positioned a /\ List Path)


transform :: forall a. Number -> Number -> Number -> Number -> Positioned a -> Positioned a
transform dx dy sx sy node =
    node { x = sx * node.x + dx, y = sy * node.y + dy, width = sx * node.width, height = sy * node.height }


scale :: forall a. Number -> Positioned a -> Positioned a
scale factor =
    transform 0.0 0.0 factor factor


distributePositions :: forall a. Geometry -> (Path -> a -> Size) -> Graph Path a -> Graph Path (Positioned a)
distributePositions geom getSize graph =
    graphMap
        # mapWithIndex ((/\))
        # foldl (flip distribute) Map.empty
        # fillBackChildren
        # map (lmap $ scale $ min geom.scaleLimit.max $ max geom.scaleLimit.min geom.scaleFactor)
        -- TODO: rotate children using grand-parent -> parent vector
        # Graph.fromMap

    where
        graphMap = Graph.toMap graph
        zeroPos = { x : 0.0, y : 0.0 }
        childPos parentPos =
            findPosition geom.baseRadius parentPos 0.0 (2.0 * Number.pi / 3.0)
        injectRect value pos size =
            { x : pos.x, y : pos.y, width : size.width, height : size.height, value }

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
                        # storeRect curPath curValue zeroPos (getSize curPath curValue)
                        # positionChildrenFrom zeroPos childPaths

        storeRect :: Path -> a -> Position -> Size -> PositionedMap a -> PositionedMap a
        storeRect path value position size =
            Map.insert path $ injectRect value position size

        positionChildrenFrom :: Position -> List Path -> PositionedMap a -> PositionedMap a
        positionChildrenFrom parentPos childPaths =
            foldl' (flip $ insertChildPos parentPos $ List.length childPaths) $ mapWithIndex (/\) (List.reverse childPaths)

        insertChildPos :: Position -> Int -> Int /\ Path -> PositionedMap a -> PositionedMap a
        insertChildPos parentPos childrenCount (index /\ childPath) prevPositions =
            case graphMap # Map.lookup childPath <#> Tuple.fst of
                Just childValue ->
                    prevPositions
                        # storeRect childPath childValue (childPos parentPos childrenCount index) (getSize childPath childValue)
                Nothing ->
                    prevPositions


type Config a =
    { valueLabel :: Path -> a -> String
    , valueColor :: Path -> a -> HSA.Color
    , edgeColor :: Path -> a -> Path -> a -> HSA.Color
    , edgeLabel :: Path -> a -> Path -> a -> String
    , valueSize :: Path -> a -> { width :: Number, height :: Number }
    }


type Events i a =
    { valueClick :: Path -> a -> i
    , valueOver  :: Path -> a -> i
    , valueOut   :: Path -> a -> i
    }


type Geometry =
    { scaleFactor :: Number
    , baseRadius :: Number
    , scaleLimit :: { min :: Number, max :: Number }
    }


renderGraph :: forall a p i. Geometry -> Config a -> Events i a -> Graph Path a -> Array (HTML p i)
renderGraph geom config events graph = foldl (<>) [] $ mapWithIndex renderNode positionsMap
    where
        positionsMap :: PositionedGraphMap a
        positionsMap = Graph.toMap $ distributePositions geom config.valueSize graph
        renderValue nodePath { x, y, value } =
            HS.g
                [ HSA.transform $ pure $ HSA.Translate x y
                , HHP.style "cursor: pointer; pointer-events: all;"
                , HE.onClick     $ const $ events.valueClick nodePath value
                , HE.onMouseOver $ const $ events.valueOver  nodePath value
                , HE.onMouseOut  $ const $ events.valueOut   nodePath value
                ]
                [ HS.circle
                    [ HSA.cx 0.0
                    , HSA.cy 0.0
                    , HSA.r 5.0
                    , HSA.fill $ config.valueColor nodePath value
                    , HSA.stroke $ HSA.RGB 0 0 0
                    ]
                , HS.text
                    [ HSA.fill $ config.valueColor nodePath value
                    , HSA.x 6.0
                    , HSA.y 9.0
                    ]
                    [ HH.text $ config.valueLabel nodePath value ]
                ]
        renderEdge parentPath parent childPath =
            case Map.lookup childPath positionsMap <#> Tuple.fst of
                Just child ->
                    HS.g
                        [ HHP.style "cursor: cross; pointer-events: none;" ]
                        [ HS.line
                            [ HSA.x1 parent.x
                            , HSA.y1 parent.y
                            , HSA.x2 child.x
                            , HSA.y2 child.y
                            , HSA.stroke $ config.edgeColor parentPath parent.value childPath child.value
                            ]
                        , HS.text
                            [ HSA.x $ parent.x + ((child.x - parent.x) / 2.0)
                            , HSA.y $ parent.y + ((child.y - parent.y) / 2.0)
                            ]
                            [ HH.text $ config.edgeLabel parentPath parent.value childPath child.value ]
                        ]
                Nothing -> HS.g [] []
        renderNode nodePath (a /\ paths) =
            (renderEdge nodePath a <$> Array.fromFoldable paths)
            <> [ renderValue nodePath a ]


renderPreview :: forall a p i. Config a -> Path -> a -> HTML p i
renderPreview config nodePath value =
    let
        previewSize = config.valueSize nodePath value
    in
        HS.svg
            [ HSA.width  $ previewSize.width
            , HSA.height $ previewSize.height
            ]
            $ pure $ HS.g
                [ HHP.style "pointer-events: none;"
                ]
                [ HS.circle
                    [ HSA.cx $ previewSize.width / 2.0
                    , HSA.cy $ previewSize.height / 2.0
                    , HSA.r 5.0
                    , HSA.fill $ config.valueColor nodePath value
                    , HSA.stroke $ HSA.RGB 0 0 0
                    ]
                , HS.text
                    [ HSA.fill $ config.valueColor nodePath value
                    , HSA.x $ (previewSize.width / 2.0) + 6.0
                    , HSA.y $ (previewSize.height / 2.0) + 9.0
                    ]
                    [ HH.text $ config.valueLabel nodePath value ]
                ]