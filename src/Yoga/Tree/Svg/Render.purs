module Yoga.Tree.Svg.Render
    ( Slots, NodeSlot, _item, _preview
    , GraphStatus(..), NodeMode(..), EdgeMode(..), NodeStatus(..)
    , NodeQuery, NodeOutput
    , Modes, Config, Events, Geometry
    , NodeComponent, NodeComponentInput, GraphHtml
    , renderGraph, renderGraph_, renderGraph'
    , renderPreview, renderPreview_, renderPreview'
    , WithStatus, statusColor
    )
    where

import Prelude

import Type.Proxy (Proxy(..))

import Prim.Row (class Cons) as Row

import Data.Symbol (class IsSymbol)
import Data.Number (pi) as Number
import Data.Int (toNumber) as Int
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Graph (Graph, Edge)
import Data.Graph (toMap, fromMap, lookup) as Graph
import Data.Map (empty, lookup, insert, update) as Map
import Data.Array (fromFoldable, toUnfoldable) as Array
import Data.Tuple (fst, snd) as Tuple
import Data.List (List(..))
import Data.List (length, reverse) as List
import Data.Foldable (class Foldable, foldl)
import Data.FunctorWithIndex (mapWithIndex)
import Data.Tuple.Nested ((/\), type (/\))
import Data.Bifunctor (lmap)

import Yoga.Tree.Extended.Path (Path)
import Yoga.Tree.Extended.Path (up, lastPos) as Path

import Yoga.Tree.Svg.Geometry (Position, Positioned, PositionedGraphMap, PositionedMap, Size, findPosition, scale, rotateBy)
import Yoga.Tree.Svg.Style as Style
import Yoga.Tree.Svg.Style (Mode(..))

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


data GraphStatus
    = GKeysFocus
    | GHoverFocus
    | GGhostFocus
    | GNormal


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


derive instance Eq NodeStatus


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
            findPosition geom.baseDistance parentPos 0.0 (2.0 * Number.pi / 3.0)
        injectRect value pos size =
            { x : pos.x, y : pos.y, width : size.width, height : size.height, value }
        calcTheta rotData =
            let
                angleStart = -1.0 * (total / 2.0)
                angleEnd   =  1.0 * (total / 2.0)
                total = neighNum * (Number.pi / 10.0)
                neighNum = Int.toNumber rotData.neighbours
                posNum = Int.toNumber rotData.pos
            in -1.0 * (angleStart + (posNum / (neighNum - 1.0)) * total)

        inParent :: Path -> { neighbours :: Int, pos :: Int }
        inParent path =
            { neighbours : Map.lookup (Path.up path) graphMap <#> Tuple.snd <#> List.length # fromMaybe 0
            , pos : Path.lastPos path # fromMaybe (-1)
            }

        fillBackChildren :: PositionedMap a -> PositionedGraphMap a
        fillBackChildren = mapWithIndex \path cell -> cell /\ (Map.lookup path graphMap <#> Tuple.snd # fromMaybe Nil)

        distribute :: Path /\ a /\ List Path -> PositionedMap a -> PositionedMap a
        distribute (curPath /\ curValue /\ childPaths) prevPositions =
            case prevPositions # Map.lookup curPath of
                Nothing -> -- we didn't visit `curPath` as a child of some other node: it's probably a root node
                    prevPositions
                            # storeRect curPath curValue zeroPos (getSize curPath curValue)
                            # positionChildrenFrom zeroPos childPaths -- for now, position children from zero position
                Just { x, y } -> -- we did located `curPath` as a child of some other node before, we can position its chidren relatively
                    let
                        rotData = inParent curPath
                    in prevPositions
                        # positionChildrenFrom { x, y } childPaths
                        # rotateAll { x, y } (calcTheta rotData) childPaths

        storeRect :: Path -> a -> Position -> Size -> PositionedMap a -> PositionedMap a
        storeRect path value position size =
            Map.insert path $ injectRect value position size

        positionChildrenFrom :: Position -> List Path -> PositionedMap a -> PositionedMap a
        positionChildrenFrom parentPos childPaths =
            foldl' (flip $ insertChildPos parentPos $ List.length childPaths) $ mapWithIndex (/\) (List.reverse childPaths)

        rotateAll :: Position -> Number -> List Path -> PositionedMap a -> PositionedMap a
        rotateAll parentPos theta childPaths =
            foldl'
                (\positions chPath ->
                    Map.update
                        (\rec ->
                            let
                                rotated = rotateBy parentPos { x : rec.x, y : rec.y  } 1.0 theta
                            in Just $ rec { x = rotated.x, y = rotated.y }
                        )
                        chPath
                        positions)
                childPaths

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
    , valueLabelColor :: Path -> a -> HSA.Color
    , valueLabelWidth :: Path -> a -> Int
    , edgeColor :: Path -> a -> Path -> a -> HSA.Color
    , edgeLabel :: Path -> a -> Path -> a -> String
    , componentSize :: Path -> a -> { width :: Number, height :: Number }
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
    -- TODO maximum amount of childNodes
    }


type Geometry =
    { scaleFactor :: Number
    , baseDistance :: Number
    , valueRadius :: Number -- = 5.0 :: Number
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


renderGraph :: forall a i m. GraphStatus -> Modes -> Geometry -> Config a -> Events i a -> Graph Path (WithStatus a) -> Array (GraphHtml m i)
renderGraph gstatus modes geom config = renderGraph' gstatus modes geom config Nothing


renderGraph_ :: forall a i m. GraphStatus -> Modes -> Geometry -> Config a -> NodeComponent m a -> Events i a -> Graph Path (WithStatus a) -> Array (GraphHtml m i)
renderGraph_ gstatus modes geom config childComp = renderGraph' gstatus modes geom config (Just childComp)


renderGraph' :: forall a i m. GraphStatus -> Modes -> Geometry -> Config a -> Maybe (NodeComponent m a) -> Events i a -> Graph Path (WithStatus a) -> Array (GraphHtml m i)
renderGraph' gstatus modes geom config mbComponent events graph = foldl (<>) [] $ mapWithIndex renderNode positionsMap
    where
        statusOf :: Path -> NodeStatus
        statusOf path = Graph.lookup path graph <#> _statusOf # fromMaybe Normal
        valuesGraph :: Graph Path a
        valuesGraph = graph <#> _valueOf
        positionsMap :: PositionedGraphMap a
        positionsMap = Graph.toMap $ distributePositions geom config.componentSize valuesGraph
        renderValue :: Path -> Positioned a -> _
        renderValue nodePath { x, y, value } =
            HS.g
                [ HSA.transform $ pure $ HSA.Translate x y
                , HHP.style Style.value
                , HE.onClick     $ const $ events.valueClick nodePath value
                , HE.onMouseOver $ const $ events.valueOver  nodePath value
                , HE.onMouseOut  $ const $ events.valueOut   nodePath value
                ]
                $ case (statusOf nodePath) of
                    KeysNext ->
                        [ _renderValue gstatus modes.nodeMode geom config _item mbComponent (statusOf nodePath) { x : 0.0, y : 0.0 } nodePath value
                        , HS.text
                            [ HSA.x $ -1.0 * (geom.valueRadius / 2.0)
                            , HSA.y $ -7.0
                            , HSA.fill $ Style.orA Light
                            ]
                            [ case Path.lastPos nodePath of -- TODO: pass index of the item instead
                                Just lastVal -> HH.text $ show lastVal
                                Nothing -> HH.text "?"
                            ]
                        ]
                    _ -> pure $ _renderValue gstatus modes.nodeMode geom config _item mbComponent (statusOf nodePath) { x : 0.0, y : 0.0 } nodePath value
        renderEdge parentPath parent childPath =
            case Map.lookup childPath positionsMap <#> Tuple.fst of
                Just child ->
                    HS.g
                        [ HHP.style Style.edge ]
                        $ pure
                        $ _renderEdge gstatus { start : statusOf parentPath, end : statusOf childPath } modes.edgeMode config
                        $
                            { start : { path : parentPath, pos : { x : parent.x, y: parent.y }, value : parent.value }
                            , end   : { path : childPath,  pos : { x : child.x,  y: child.y  }, value : child.value  }
                            }
                Nothing -> HS.g [] []
        renderNode :: Path -> (Positioned a /\ List Path) -> _
        renderNode nodePath (a /\ paths) =
            (renderEdge nodePath a <$> Array.fromFoldable paths)
            <> [ renderValue nodePath a ]


renderPreview :: forall a i m. NodeMode -> Geometry -> Config a -> NodeStatus -> Path -> a -> GraphHtml m i
renderPreview nodeMode geom config = renderPreview' nodeMode geom config Nothing


renderPreview_ :: forall a i m. NodeMode -> Geometry -> Config a -> NodeComponent m a -> NodeStatus -> Path -> a -> GraphHtml m i
renderPreview_ nodeMode geom config childComp = renderPreview' nodeMode geom config (Just childComp)


renderPreview' :: forall a i m. NodeMode -> Geometry -> Config a -> Maybe (NodeComponent m a) -> NodeStatus -> Path -> a -> GraphHtml m i
renderPreview' nodeMode geom config mbComponent nodeStatus nodePath value =
    let
        componentSize = config.componentSize nodePath value
        padding = 5.0
        position =
            case nodeMode of
              Component ->
                { x : 0.0
                , y : componentSize.height / 2.0
                }
              _ ->
                { x : componentSize.width  / 2.0
                , y : componentSize.height / 2.0
                }
    in
        HS.svg
            [ HSA.width  $ componentSize.width + padding * 2.0
            , HSA.height $ componentSize.height + padding * 2.0
            ]
            [ HS.rect
                [ HSA.width  $ componentSize.width + padding * 2.0
                , HSA.height $ componentSize.height + padding * 2.0
                , HSA.stroke $ _strokeFromStatus nodeStatus
                , HSA.strokeWidth $ _strokeWidthFromStatus nodeStatus
                , HSA.fill $ HSA.RGBA 0 0 0 0.0
                ]
            , HS.g
                [ HHP.style Style.previewBox
                , HSA.transform $ pure $ HSA.Translate padding padding
                ]
                $ pure
                $ _renderValue GNormal nodeMode geom config _preview mbComponent nodeStatus position nodePath value
            ]


statusColor :: NodeStatus -> HSA.Color
statusColor = case _ of
  Normal -> HSA.RGBA 0 0 0 0.0
  FocusRoot -> Style.alpha 0.6 $ Style.reA Light
  HoverFocus -> Style.alpha 0.6 $ Style.grB Light
  HoverGhost -> Style.alpha 0.6 $ Style.grA Light
  Selected -> Style.alpha 0.6 $ Style.blB Light
  KeysNext -> Style.alpha 0.6 $ Style.blA Light
  _ -> HSA.RGBA 0 0 0 0.0


type EdgeJoint a =
    { path :: Path
    , value :: a
    , pos :: Position
    }


type EdgeDef a = Edge (EdgeJoint a)


type EdgeStatus = Edge NodeStatus


_renderEdge :: forall a p i. GraphStatus -> EdgeStatus -> EdgeMode -> Config a -> EdgeDef a -> HTML p i
_renderEdge gstatus estatus EdgeWithLabel config edge =
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
_renderEdge gstatus estatus JustEdge config edge =
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
            , HSA.strokeOpacity $ _edgeOpacity gstatus estatus
            ]
        ]

_renderValue
    :: forall slot r' i m a
     . IsSymbol slot
    => Row.Cons slot NodeSlot r' Slots
    => GraphStatus
    -> NodeMode
    -> Geometry
    -> Config a
    -> Proxy slot
    -> Maybe (NodeComponent m a)
    -> NodeStatus
    -> Position
    -> Path
    -> a
    -> GraphHtml m i
_renderValue gstatus NodeWithLabel geom config _ _ status pos nodePath value =
    HS.g
        []
        [ HS.circle
            [ HSA.cx pos.x
            , HSA.cy pos.y
            , HSA.r geom.valueRadius
            , HSA.fill          $ config.valueColor nodePath value
            , HSA.stroke        $ _strokeFromStatus status
            , HSA.strokeWidth   $ _strokeWidthFromStatus status
            , HSA.fillOpacity   $ _nodeOpacity gstatus status
            , HSA.strokeOpacity $ _nodeOpacity gstatus status
            ]
        , HS.rect
            [ HSA.fill $ Style.blA Light
            , HSA.x $ pos.x
            , HSA.y $ pos.y - 2.0
            , HSA.rx 6.0
            , HSA.ry 6.0
            , HSA.width $ Int.toNumber (config.valueLabelWidth nodePath value) * 8.0 + 5.0
            , HSA.height 15.0
            , HSA.fillOpacity $ case status of
                Normal -> 0.1
                FocusRoot -> 0.0
                KeysFocus -> 0.6
                KeysNext -> 0.35
                HoverFocus -> 0.3
                HoverGhost -> 0.2
                Selected -> 0.6
                Pinned -> 0.0
                _ -> 0.1
            ]
        , HS.text
            [ HSA.fill $ config.valueLabelColor nodePath value
            , HSA.x $ pos.x + 6.0
            , HSA.y $ pos.y + 9.0
            ]
            [ HH.text $ config.valueLabel nodePath value
            ]
        ]
_renderValue gstatus JustNode geom config _ _ status pos nodePath value =
    HS.g
        []
        [ HS.circle
            [ HSA.cx pos.x
            , HSA.cy pos.y
            , HSA.r geom.valueRadius
            , HSA.fill          $ config.valueColor nodePath value
            , HSA.stroke        $ _strokeFromStatus status
            , HSA.strokeWidth   $ _strokeWidthFromStatus status
            , HSA.fillOpacity   $ _nodeOpacity gstatus status
            , HSA.strokeOpacity $ _nodeOpacity gstatus status
            ]
        ]
_renderValue gstatus Component _ _ _ Nothing status pos _ _ =
    HS.text
        [ HSA.x pos.x, HSA.y pos.y
        , HSA.fill $ HSA.RGB 0 0 0
        ]
        [ HH.text "NO COMPONENT" ]
_renderValue gstatus Component _ _ pslot (Just childComp) status pos nodePath value =
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
  Normal -> Style.tx Light
  FocusRoot -> Style.reA Light
  HoverFocus -> Style.grB Light
  HoverGhost -> Style.grA Light
  Selected -> Style.blB Light
  KeysNext -> Style.blA Light
  _ -> HSA.RGB 0 0 0


_strokeWidthFromStatus :: NodeStatus -> Number
_strokeWidthFromStatus  = case _ of
  Normal -> 1.0
  FocusRoot -> 2.0
  HoverFocus -> 3.0
  HoverGhost -> 2.0
  Selected -> 3.0
  KeysNext -> 2.0
  _ -> 1.0


_nodeOpacity :: GraphStatus -> NodeStatus -> Number
_nodeOpacity GNormal _ = 1.0
_nodeOpacity GKeysFocus FocusRoot = 0.6
_nodeOpacity GKeysFocus KeysFocus = 1.0
_nodeOpacity GKeysFocus KeysNext = 1.0
_nodeOpacity GKeysFocus Selected = 1.0
_nodeOpacity GKeysFocus _ = 0.4
_nodeOpacity GHoverFocus FocusRoot = 1.0
_nodeOpacity GHoverFocus KeysFocus = 0.7
_nodeOpacity GHoverFocus KeysNext = 0.7
_nodeOpacity GHoverFocus Selected = 1.0
_nodeOpacity GHoverFocus _ = 0.5
_nodeOpacity GGhostFocus _ = 1.0


_oneOf :: NodeStatus -> EdgeStatus -> Boolean
_oneOf ns { start, end } = ns == start || ns == end

_startOf :: NodeStatus -> EdgeStatus -> Boolean
_startOf ns { start } = ns == start

_endOf :: NodeStatus -> EdgeStatus -> Boolean
_endOf ns { start } = ns == start


_edgeOpacity :: GraphStatus -> EdgeStatus -> Number
_edgeOpacity GNormal _ = 1.0
_edgeOpacity GKeysFocus estatus | _oneOf KeysFocus estatus = 1.0
_edgeOpacity GKeysFocus estatus | _oneOf Selected estatus = 1.0
_edgeOpacity GKeysFocus estatus | _startOf KeysNext estatus = 0.6
_edgeOpacity GKeysFocus estatus | _endOf KeysNext estatus = 1.0
_edgeOpacity GKeysFocus estatus | _oneOf FocusRoot estatus = 0.3
_edgeOpacity GKeysFocus _ = 0.1
_edgeOpacity GHoverFocus estatus | _oneOf HoverFocus estatus = 1.0
_edgeOpacity GHoverFocus estatus | _oneOf HoverGhost estatus = 1.0
_edgeOpacity GHoverFocus estatus | _oneOf Selected estatus = 1.0
_edgeOpacity GHoverFocus estatus | _oneOf FocusRoot estatus = 0.3
_edgeOpacity GHoverFocus estatus | _oneOf KeysFocus estatus = 0.3
_edgeOpacity GHoverFocus estatus | _oneOf KeysNext estatus = 0.3
_edgeOpacity GHoverFocus _ = 0.1
_edgeOpacity GGhostFocus _ = 1.0
