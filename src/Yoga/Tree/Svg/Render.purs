module Yoga.Tree.Svg.Render
    ( Slots, NodeSlot, _item, _preview
    , NodeQuery, NodeOutput
    , NodeMode(..), EdgeMode(..), NodeStatus(..)
    , Modes, Config, Events, Geometry
    , NodeComponent, NodeComponentInput, GraphHtml
    , renderGraph, renderGraph_, renderGraph'
    , renderPreview, renderPreview_, renderPreview'
    , WithStatus
    )
    where

import Prelude

import Type.Proxy (Proxy(..))

import Prim.Row (class Cons) as Row

import Data.Symbol (class IsSymbol)
import Data.Number (pi) as Number
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Graph (Graph, Edge)
import Data.Graph (toMap, fromMap, lookup) as Graph
import Data.Map (empty, lookup, insert) as Map
import Data.Array (fromFoldable, toUnfoldable) as Array
import Data.Tuple (fst, snd) as Tuple
import Data.List (List(..))
import Data.List (length, reverse) as List
import Data.Foldable (class Foldable, foldl)
import Data.FunctorWithIndex (mapWithIndex)
import Data.Tuple.Nested ((/\), type (/\))
import Data.Bifunctor (lmap)

import Yoga.Tree.Extended.Path (Path)

import Yoga.Tree.Svg.Geometry (Position, Positioned, PositionedGraphMap, PositionedMap, Size, findPosition, scale)

import Halogen as H
import Halogen.HTML (HTML)
import Halogen.HTML as HH
import Halogen.HTML.Properties as HHP
import Halogen.HTML.Events as HE
import Halogen.Svg.Attributes as HSA
import Halogen.Svg.Elements as HS


type NodeSlot = H.Slot NodeQuery NodeOutput Path


type Slots =
  ( item    :: NodeSlot
  , preview :: NodeSlot
  , pinned  :: NodeSlot
  )


_item  = Proxy :: _ "item"
_preview  = Proxy :: _ "preview"
_pinned  = Proxy :: _ "pinned"


data NodeQuery x = NodeQuery
type NodeOutput = Void


data NodeMode
    = JustNode
    | NodeWithLabel
    | Component


data EdgeMode
    = JustEdge
    | EdgeWithLabel


data NodeStatus
    = Normal
    | FocusRoot -- when the node is in the root of current navigation
    | KeysFocus -- when user focused over the node with keys
    | KeysNext -- when node is the candidate for being selected next
    | HoverFocus -- when node is hovered with mouse cursor (preview)
    | HoverGhost -- when cursor left the node but no other node was hovered over (ghost preview)
    | Selected -- when node is the part of current selection
    | Pinned -- when node is pinned
    | Combo NodeStatus NodeStatus


foldl' :: forall f a b. Foldable f => (b -> a -> b) -> f a -> b -> b
foldl' = flip <<< foldl


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


type Modes =
    { nodeMode :: NodeMode
    , edgeMode :: EdgeMode
    , previewMode :: NodeMode
    }


type Geometry =
    { scaleFactor :: Number
    , baseRadius :: Number
    , scaleLimit :: { min :: Number, max :: Number }
    }


type NodeComponent m a
    = H.Component NodeQuery (NodeComponentInput a) NodeOutput m


type NodeComponentInput a =
    { path :: Path
    , value :: a
    }


type GraphHtml m i  = HTML (H.ComponentSlot Slots m i) i


type WithStatus a = NodeStatus /\ a


_statusOf :: forall a. WithStatus a -> NodeStatus
_statusOf = Tuple.fst


_valueOf :: forall a. WithStatus a -> a
_valueOf = Tuple.snd


renderGraph :: forall a i m. Modes -> Geometry -> Config a -> Events i a -> Graph Path (WithStatus a) -> Array (GraphHtml m i)
renderGraph modes geom config = renderGraph' modes geom config Nothing


renderGraph_ :: forall a i m. Modes -> Geometry -> Config a -> NodeComponent m a -> Events i a -> Graph Path (WithStatus a) -> Array (GraphHtml m i)
renderGraph_ modes geom config childComp = renderGraph' modes geom config (Just childComp)


renderGraph' :: forall a i m. Modes -> Geometry -> Config a -> Maybe (NodeComponent m a) -> Events i a -> Graph Path (WithStatus a) -> Array (GraphHtml m i)
renderGraph' modes geom config mbComponent events graph = foldl (<>) [] $ mapWithIndex renderNode positionsMap
    where
        statusOf :: Path -> NodeStatus
        statusOf path = Graph.lookup path graph <#> _statusOf # fromMaybe Normal
        valuesGraph :: Graph Path a
        valuesGraph = graph <#> _valueOf
        positionsMap :: PositionedGraphMap a
        positionsMap = Graph.toMap $ distributePositions geom config.valueSize valuesGraph
        renderValue :: Path -> Positioned a -> _
        renderValue nodePath { x, y, value } =
            HS.g
                [ HSA.transform $ pure $ HSA.Translate x y
                , HHP.style "cursor: pointer; pointer-events: all;"
                , HE.onClick     $ const $ events.valueClick nodePath value
                , HE.onMouseOver $ const $ events.valueOver  nodePath value
                , HE.onMouseOut  $ const $ events.valueOut   nodePath value
                ]
                $ pure
                $ _renderValue modes.nodeMode config _item mbComponent (statusOf nodePath) { x : 0.0, y : 0.0 } nodePath value
        renderEdge parentPath parent childPath =
            case Map.lookup childPath positionsMap <#> Tuple.fst of
                Just child ->
                    HS.g
                        [ HHP.style "cursor: cross; pointer-events: none;" ]
                        $ pure
                        $ _renderEdge modes.edgeMode config
                        $
                            { start : { path : parentPath, pos : { x : parent.x, y: parent.y }, value : parent.value }
                            , end   : { path : childPath,  pos : { x : child.x,  y: child.y  }, value : child.value  }
                            }
                Nothing -> HS.g [] []
        renderNode :: Path -> (Positioned a /\ List Path) -> _
        renderNode nodePath (a /\ paths) =
            (renderEdge nodePath a <$> Array.fromFoldable paths)
            <> [ renderValue nodePath a ]


renderPreview :: forall a i m. NodeMode -> Config a -> NodeStatus -> Path -> a -> GraphHtml m i
renderPreview nodeMode config = renderPreview' nodeMode config Nothing


renderPreview_ :: forall a i m. NodeMode -> Config a -> NodeComponent m a -> NodeStatus -> Path -> a -> GraphHtml m i
renderPreview_ nodeMode config childComp = renderPreview' nodeMode config (Just childComp)


renderPreview' :: forall a i m. NodeMode -> Config a -> Maybe (NodeComponent m a) -> NodeStatus -> Path -> a -> GraphHtml m i
renderPreview' nodeMode config mbComponent nodeStatus nodePath value =
    let
        previewSize = config.valueSize nodePath value
        position =
            case nodeMode of
              Component ->
                { x : 0.0
                , y : previewSize.height / 2.0
                }
              _ ->
                { x : previewSize.width / 2.0
                , y : previewSize.height / 2.0
                }
    in
        HS.svg
            [ HSA.width  $ previewSize.width
            , HSA.height $ previewSize.height
            ]
            [ HS.rect
                [ HSA.width  $ previewSize.width
                , HSA.height $ previewSize.height
                , HSA.stroke $ _strokeFromStatus nodeStatus
                , HSA.fill $ HSA.RGBA 0 0 0 0.0
                ]
            , HS.g
                [ HHP.style "pointer-events: none;"
                ]
                $ pure
                $ _renderValue nodeMode config _preview mbComponent nodeStatus position nodePath value
            ]


type EdgeJoint a =
    { path :: Path
    , value :: a
    , pos :: Position
    }


type EdgeDef a = Edge (EdgeJoint a)


_renderEdge :: forall a p i. EdgeMode -> Config a -> EdgeDef a -> HTML p i
_renderEdge EdgeWithLabel config edge =
    let
        parent = edge.start
        child = edge.end
        parentPos = parent.pos
        childPos= child.pos
    in HS.g []
        [ HS.line
            [ HSA.x1 parentPos.x
            , HSA.y1 parentPos.y
            , HSA.x2 childPos.x
            , HSA.y2 childPos.y
            , HSA.stroke $ config.edgeColor parent.path parent.value child.path child.value
            ]
        , HS.text
            [ HSA.x $ parentPos.x + ((childPos.x - parentPos.x) / 2.0)
            , HSA.y $ parentPos.y + ((childPos.y - parentPos.y) / 2.0)
            ]
            [ HH.text $ config.edgeLabel parent.path parent.value child.path child.value ]
        ]
_renderEdge JustEdge config edge =
    let
        parent = edge.start
        child = edge.end
        parentPos = parent.pos
        childPos= child.pos
    in HS.g []
        [ HS.line
            [ HSA.x1 parentPos.x
            , HSA.y1 parentPos.y
            , HSA.x2 childPos.x
            , HSA.y2 childPos.y
            , HSA.stroke $ config.edgeColor parent.path parent.value child.path child.value
            ]
        ]

_renderValue
    :: forall slot r' i m a
     . IsSymbol slot
    => Row.Cons slot NodeSlot r' Slots
    => NodeMode
    -> Config a
    -> Proxy slot
    -> Maybe (NodeComponent m a)
    -> NodeStatus
    -> Position
    -> Path
    -> a
    -> GraphHtml m i
_renderValue NodeWithLabel config _ _ status pos nodePath value =
    HS.g
        []
        [ HS.circle
            [ HSA.cx pos.x
            , HSA.cy pos.y
            , HSA.r 5.0
            , HSA.fill $ config.valueColor nodePath value
            , HSA.stroke $ _strokeFromStatus status
            ]
        , HS.text
            [ HSA.fill $ config.valueColor nodePath value
            , HSA.x $ pos.x + 6.0
            , HSA.y $ pos.y + 9.0
            ]
            [ HH.text $ config.valueLabel nodePath value
            ]
        ]
_renderValue JustNode config _ _ status pos nodePath value =
    HS.g
        []
        [ HS.circle
            [ HSA.cx pos.x
            , HSA.cy pos.y
            , HSA.r 5.0
            , HSA.fill $ config.valueColor nodePath value
            , HSA.stroke $ _strokeFromStatus status
            ]
        ]
_renderValue Component _ _ Nothing status pos _ _ =
    HS.text
        [ HSA.x pos.x, HSA.y pos.y
        , HSA.fill $ HSA.RGB 0 0 0
        ]
        [ HH.text "NO COMPONENT" ]
_renderValue Component _ pslot (Just childComp) status pos nodePath value =
    HS.g
        [ HSA.transform $ pure $ HSA.Translate pos.x pos.y ]
        $ pure
        $ HH.slot_
            pslot
            nodePath
            childComp
            { path : nodePath
            , value
            }


_strokeFromStatus :: NodeStatus -> HSA.Color
_strokeFromStatus = case _ of
  Normal -> HSA.RGB 0 0 0
  FocusRoot -> HSA.RGB 0 70 0
  HoverFocus -> HSA.RGB 255 0 0
  HoverGhost -> HSA.RGB 170 0 0
  Selected -> HSA.RGB 0 0 255
  KeysNext -> HSA.RGB 0 255 0
  _ -> HSA.RGB 0 0 0
