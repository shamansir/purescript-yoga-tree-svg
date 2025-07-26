module Yoga.Tree.Svg
  ( NodeComponent
  , component, component_, init
  , module LayoutEx
  , Query(..), Output(..), Slot
  , ExportMode(..)
  , allExports
  ) where

import Prelude

import Data.Array ((:))
import Data.Array (take, fromFoldable, dropEnd, length, snoc, last, reverse, replicate, groupBy, range, drop) as Array
import Data.Array.NonEmpty as NEA
import Data.Bifunctor (lmap)
import Data.Foldable (foldl)
import Data.FunctorWithIndex (mapWithIndex)
import Data.Graph (Graph)
import Data.Graph (fromMap, toMap) as Graph
import Data.Int (floor, pow) as Int
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Set (Set)
import Data.Set (empty, insert, delete, member, size, fromFoldable, toUnfoldable) as Set
import Data.String (take, toLower, joinWith) as String
import Data.Tuple (fst, snd) as Tuple
import Data.Tuple.Nested ((/\))

import Effect.Class (class MonadEffect)

import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Query.Event (eventListener)
import Halogen.Svg.Attributes as HSA
import Halogen.Svg.Elements as HS

import Play as Play

import Web.Event.Event as E
import Web.HTML (window)
import Web.HTML.HTMLDocument as HTMLDocument
import Web.HTML.Window (document)
import Web.UIEvent.KeyboardEvent as KE
import Web.UIEvent.KeyboardEvent.EventTypes as KET
import Web.UIEvent.MouseEvent as ME
import Web.UIEvent.WheelEvent as WE

import Yoga.Tree (Tree)
import Yoga.Tree.Extended (value, children) as Tree
import Yoga.Tree.Extended.Convert as TreeConv
import Yoga.Tree.Extended.Convert.Dot as TreeConv
import Yoga.Tree.Extended.Graph (toGraph') as Tree
import Yoga.Tree.Extended.Path (Path(..))
import Yoga.Tree.Extended.Path (find, root, toArray, depth, safeAdvance, Dir(..), advanceDir, isNextFor) as Path
import Yoga.Tree.Svg.Layout (Element(..), all) as LayoutEx
import Yoga.Tree.Svg.Layout as L
import Yoga.Tree.Svg.Render (WithStatus, statusColor)
import Yoga.Tree.Svg.Render as SvgTree
import Yoga.Tree.Svg.Style as Style
import Yoga.Tree.Svg.SvgItem (class IsSvgTreeItem)
import Yoga.Tree.Svg.SvgItem (foldLabel, pinnedLine, _export, _import, default) as SvgI

-- import Yoga.Tree.Zipper (Path)


data Action a
    = Initialize
    | Receive (Input a)
    | WheelChange { dx :: Number, dy :: Number }
    | HandleKey H.SubscriptionId KE.KeyboardEvent
    | FocusOn Path
    | NodeClick Path a
    | NodeOver  Path a
    | NodeOut   Path a
    | PreviewOver Path
    | ResetZoom
    | Pin Path
    | UnPin Path
    | PinScroll Int
    | TryParse String
    | PauseListeningKeys
    | ResumeListeningKeys
    | EnterTreeTextMode
    | LeaveTreeTextMode
    | EnterPinTextMode
    | LeavePinTextMode
    | SwitchExport ExportMode
    | FoldSelect ME.MouseEvent Path
    | ToggleFoldMode
    | BreadcrumbsAction Path
    | DoNothing


data PathMode
    = Breadcrumbs { withAction :: Boolean }
    | ReadOnly
    | SingleGo


data FoldMode
    = All
    | OnlyFocus


-- data PathItemMode
--     = PIReadOnly
--     | PINav


data ExportMode
    = Json
    | Text TreeConv.Mode
    | Dot


type Input a =
    { tree :: Tree a
    , size :: { width :: Number, height :: Number }
    , elements :: Set L.Element
    , exports :: Set ExportMode
    , theme :: Style.Theme
    , depthLimit :: SvgTree.SoftLimit
    , childrenLimit :: SvgTree.SoftLimit
    , mbFocus :: Maybe Path
    , breadcrumbsAction :: Boolean
    , showChildrenCount :: Boolean
    , foldColumnWidth :: Maybe Number
    }


data Preview
    = None
    | Focused Path
    | LostFocus Path


type State a =
    { tree :: Tree a
    , focus :: Path
    , history :: Array Path
    , preview :: Preview
    , pinned :: Set Path
    , zoom :: Number
    , size :: { width :: Number, height :: Number }
    , selection :: Maybe Path
    , treeTextMode :: Boolean
    , pinnedTextMode :: Boolean
    , editingSomeText :: Boolean
    , elements :: Set L.Element
    , exports :: Set ExportMode
    , theme :: Style.Theme
    , exportMode :: ExportMode
    , foldMode :: FoldMode
    , depthLimit :: SvgTree.SoftLimit
    , childrenLimit :: SvgTree.SoftLimit
    , numKeyBuffer :: Array Int
    , pinnedScrollTo :: Maybe Int
    , breadcrumbsAction :: Boolean
    , showChildrenCount :: Boolean
    , foldColumnWidth :: Maybe Number
    }


data PinUnpinMode_ -- FIXME: merge in an accurate way with `data Preview`, so keyboard selection never intersects with hover preview?
    = Preview -- TODO: rename `Preview` to `HoverView` / `HoverGhost`?
    | Selection
    | Pinned


data Query q
    = ChangeSelection Path q
    | ClearSelection q
    | RequestSelection (Maybe Path -> q)
    | ChangeFocus Path q
    | RequestFocus (Path -> q)


data Output
    = SelectionChanged Path
    | SelectionCleared
    | FocusChanged Path
    | BreadcrumbsActionTriggered Path


type SvgTree m a = H.Component Query (Input a) Output m
type SvgTreeM m a x = H.HalogenM (State a) (Action a) SvgTree.Slots Output m x
type Slot a = H.Slot Query Output a
type NodeComponent m a = SvgTree.NodeComponent m a


allExports :: Set ExportMode
allExports = Set.fromFoldable
    [ Json, Dot, Text TreeConv.Lines, Text TreeConv.Indent, Text TreeConv.Paths, Text TreeConv.Corners, Text TreeConv.Triangles, Text TreeConv.Dashes ]


init :: forall a. { width :: Number, height :: Number } -> Tree a -> Input a
init size tree =
    { size
    , tree
    , elements : L.all
    , exports : allExports
    , childrenLimit : SvgTree.Infinite
    , depthLimit : SvgTree.Infinite
    , mbFocus : Nothing
    , theme : Style.Light
    , breadcrumbsAction : false
    , showChildrenCount : false
    , foldColumnWidth : Nothing
    }


component :: forall a m. MonadEffect m => IsSvgTreeItem a => SvgTree.Modes -> SvgTree.RenderConfig a -> SvgTree m a
component modes rconfig = component' modes rconfig Nothing


component_ :: forall a m. MonadEffect m => IsSvgTreeItem a => SvgTree.Modes -> SvgTree.RenderConfig a -> NodeComponent m a -> SvgTree m a
component_ modes rconfig childComp = component' modes rconfig (Just childComp)


component' :: forall a m. MonadEffect m => IsSvgTreeItem a => SvgTree.Modes -> SvgTree.RenderConfig a -> Maybe (NodeComponent m a) -> SvgTree m a
component' modes rconfig mbChildComp =
  H.mkComponent
    { initialState
    , render
    , eval: H.mkEval $ H.defaultEval
        { handleAction = handleAction
        , handleQuery = handleQuery
        , receive = Just <<< Receive
        , initialize = Just Initialize
        }
    }
  where

  scaleLimit = { min : 0.2, max : 50.0 }
  defaultFoldColumnWidth = 100.0

  geometry :: State a -> SvgTree.Geometry
  geometry state =
    { scaleFactor : state.zoom * 5.0
    , baseDistance : 30.0
    , valueRadius : 5.0
    , scaleLimit
    , depthLimit : state.depthLimit
    -- , depthLimit : SvgTree.Maximum 1
    , childrenLimit : state.childrenLimit
    -- , childrenLimit : SvgTree.Maximum 5
    , showChildrenCount : state.showChildrenCount
    , foldColumnWidth : fromMaybe defaultFoldColumnWidth state.foldColumnWidth
    }

  initialState :: Input a -> State a
  initialState input =
    { tree : input.tree
    , focus : fromMaybe Path.root input.mbFocus
    , history : [ fromMaybe Path.root input.mbFocus ]
    , zoom : 1.0
    , size : input.size
    , preview : None
    , pinned : Set.empty
    , selection : Nothing
    , treeTextMode : false -- is tree text mode enabled
    , pinnedTextMode : false
    , editingSomeText : false -- are we currently editing any text (to block UI controls)
    , elements : input.elements
    , exports : input.exports
    , theme : input.theme
    , exportMode : Json
    , foldMode : OnlyFocus
    , depthLimit : input.depthLimit
    , childrenLimit : input.childrenLimit
    , numKeyBuffer : []
    , pinnedScrollTo : Nothing
    , breadcrumbsAction : input.breadcrumbsAction
    , showChildrenCount : input.showChildrenCount
    , foldColumnWidth : input.foldColumnWidth
    }


  receive :: Input a -> State a -> State a
  receive input =
    _
      { size = input.size
      , tree = input.tree
      , elements = input.elements
      , exports = input.exports
      , depthLimit = input.depthLimit
      , childrenLimit = input.childrenLimit
      , focus = fromMaybe Path.root input.mbFocus
      , theme = input.theme
      , breadcrumbsAction = input.breadcrumbsAction
      , showChildrenCount = input.showChildrenCount
      , foldColumnWidth = input.foldColumnWidth
      }

  events :: SvgTree.Events (Action a) a
  events =
    { valueClick : NodeClick
    , valueOver  : NodeOver
    , valueOut   : NodeOut
    }

  graphConfig :: State a -> SvgTree.GraphConfig a (Action a)
  graphConfig state =
    { geometry : geometry state
    , events
    , modes
    , render : rconfig
    , theme : state.theme
    }

  valueConfig :: State a -> SvgTree.NodeMode -> SvgTree.ValueConfig a
  valueConfig state nodeMode = graphConfig state # SvgTree.toValueConfig # _ { nodeMode = nodeMode }

  previewAt :: State a -> SvgTree.NodeMode -> Path -> HH.HTML _ (Action a)
  previewAt state nodeMode previewPath =
    case Path.find previewPath state.tree of
      Just previewNode ->
        SvgTree.renderPreview' (valueConfig state nodeMode) mbChildComp SvgTree.Normal previewPath $ Tree.value previewNode
      Nothing ->
        HH.text "?"

  pinnedAt :: State a -> SvgTree.NodeMode -> Path -> HH.HTML _ (Action a)
  pinnedAt state nodeMode pinnedPath =
    case Path.find pinnedPath state.tree of
      Just pinnedNode ->
        SvgTree.renderPinned' (valueConfig state nodeMode) mbChildComp SvgTree.Normal pinnedPath $ Tree.value pinnedNode
      Nothing ->
        HH.text "?"

  statusOf :: State a -> Path -> SvgTree.NodeStatus
  statusOf state path =
    let
        checkFocus =
          if state.focus == path
            then SvgTree.FocusRoot
            else SvgTree.Normal
        checkPreview =
          case state.preview of
            Focused previewPath ->
               if previewPath == path then SvgTree.HoverFocus else checkFocus
            LostFocus previewPath ->
               if previewPath == path then SvgTree.HoverGhost else checkFocus
            None -> checkFocus
        checkSelection =
          case state.selection of
            Just selPath ->
              if selPath == path
                then SvgTree.Selected
                else
                  if Path.isNextFor path selPath
                  then SvgTree.KeysNext
                  else checkPreview
            Nothing ->
              checkPreview
    in checkSelection

  injectNodeStatuses :: State a -> Graph Path a -> Graph Path (WithStatus a)
  injectNodeStatuses state =
      Graph.toMap
      >>> mapWithIndex (\path (a /\ xs) -> (statusOf state path /\ a) /\ xs)
      >>> Graph.fromMap

  render :: State a -> HH.HTML _ (Action a)
  render state =
    HH.div
      [ HP.style $ Style.component state.theme state.size.width state.size.height ]
      $ renderElement
      <$>
      (Play.flattenLayout
        $ Play.layout
        $ L.layout
            { screen : state.size
            , select : selectSize
            }
      )

    where
      defaultSelectSize = { width : 200.0, height : 60.0 }

      -- TODO: rename `Preview` to `Selection` may be, yes, `Hovered` is not a selection, but still it would have more sense,
      -- especially if we support multiple selection at some point (aren't `pinned` a multiple selection?)

      sizeOf path =
        case state.tree # Path.find path of
          Just subTree -> rconfig.componentSize path $ Tree.value subTree
          Nothing -> defaultSelectSize

      gconfig = graphConfig state :: SvgTree.GraphConfig a (Action a)

      htmlMoveTo pos =
        HH.div
          [ HP.style $ "position: absolute; left: " <> show pos.x <> "px; top: " <> show pos.y <> "px;" ]

      svgMoveTo pos =
        HS.g
            [ HSA.transform [ HSA.Translate pos.x pos.y ] ]

      bo = { withAction : state.breadcrumbsAction }

      label content = HH.span [ HP.style $ Style.txLabel state.theme ] [ HH.text content ]

      renderElement lo = htmlMoveTo lo.rect.pos $ pure $ case lo.v of

        {- Graph component -}
        L.E L.Graph ->
          HH.div
            [ HP.style Style.graph ]
            $ pure
            $ HS.svg
              [ HSA.width  lo.rect.size.width
              , HSA.height lo.rect.size.height
              , HE.onWheel \wevt -> WheelChange
                  { dx : WE.deltaX wevt
                  , dy : WE.deltaY wevt
                  }
              ]
              $ pure $ svgMoveTo { x : lo.rect.size.width / 2.0, y : 10.0 }
              $ SvgTree.renderGraphFrom'
                  { root : state.focus
                  , current : fromMaybe Path.root state.selection
                  }
                  (_graphStatus state)
                  gconfig
                  mbChildComp
                  events
                -- FIXME: passing `state.focus` is needed only because else we would first fill already focused `Tree` with `Paths` when converting it to `Graph`
                $ injectNodeStatuses state
                $ Tree.toGraph' state.focus
                $ fromMaybe state.tree
                $ Path.find state.focus state.tree

        {- Path Breadcrumbs -}
        L.E L.Breadcrumbs ->
          HH.div
          -- [ HP.style "position: absolute; right: 0; top: 20px;" ]
            [ HP.style $ case state.selection of
              Just _  -> Style.breadcrumbsWithSelection
              Nothing -> Style.breadcrumbs
            ]
            [ label "Location: "
            , renderPath state.theme (Breadcrumbs bo) gconfig.render state.tree state.focus
            , case state.selection of
                Just selPath ->
                  HH.div
                    []
                    [ label "Selection:"
                    , renderPath state.theme (Breadcrumbs bo) gconfig.render state.tree selPath
                    ]
                Nothing ->
                  HH.text ""
            , if Array.length state.numKeyBuffer > 0 then
                HH.div [] [ HH.text $ String.joinWith "" $ show <$> Array.reverse state.numKeyBuffer ]
              else HH.text ""
            ]

        {- Current Preview or Selection -}
        L.E L.PreviewOrSelection ->
          HH.div
            [ HP.style $ case state.preview of -- FIXME: xxx: merge the similar logic: `Preview` and `Selection` in one `State` field (but remember that we also need the `Selection` path for keyboard purposes only)
                Focused _   -> Style.previewFocused
                LostFocus _ ->
                  case state.selection of
                    Just _  -> Style.previewFocused
                    Nothing -> Style.previewBlurred
                None ->
                  case state.selection of
                      Just _ -> Style.previewFocused
                      Nothing -> Style.previewNone
            , HE.onMouseOver \_ -> case state.preview of
                Focused path   -> PreviewOver path
                LostFocus path -> PreviewOver path
                _ -> DoNothing
            ]
            $ pure
            $ let
                renderPreview pupMode previewPath = -- never `Pinned` here so the Halogen slot should not conflict with `Preview`/`Selection` anymore
                    wrapPinUnpin state modes.previewMode gconfig.render pupMode previewPath
            in case state.preview of -- FIXME: xxx: merge the similar logic: `Preview` and `Selection` in one `State` field (but remember that we also need the `Selection` path for keyboard purposes only)
                Focused focusPreviewPath ->
                   renderPreview Preview focusPreviewPath
                LostFocus blurredPreviewPath ->
                   case state.selection of
                      Just selectionPath ->
                          renderPreview Selection selectionPath
                      Nothing ->
                          renderPreview Preview blurredPreviewPath
                None ->
                  case state.selection of
                      Just selectionPath ->
                        renderPreview Selection selectionPath
                      Nothing ->
                          HH.text ""

        {- History -}
        L.E L.History ->
          HH.div
            [ HP.style $ Style.historyBox state.theme
            ]
            $ (HH.div [] <<< pure <<< renderPath state.theme SingleGo gconfig.render state.tree)
            <$> Array.reverse state.history

        {- Current Pinned -}
        L.E L.Pinned ->
          let
            pinnedCount = Set.size state.pinned
            pinnedLabel = if pinnedCount > 0 then show pinnedCount <> " pinned:" else ""
            pinScrollButton n = _qbutton state.theme (show $ n + 1) $ PinScroll n
            checkScroll Nothing pinnedArr = pinnedArr
            checkScroll (Just n) pinnedArr = Array.drop n pinnedArr
            -- buttonAction =
          in HH.div
            [ HP.style $ Style.pinnedBox lo.rect.size.height
            ]
             $ HH.div
                [ HP.style Style.pinnedLabel ]
                [ HH.text pinnedLabel ]

             : HH.div
                [ HP.style Style.pinnedEdit ]
                [ if pinnedCount > 0 then
                    if not state.pinnedTextMode then
                      _qbutton state.theme "Text" EnterPinTextMode -- $ if state.pinnedTextMode then EnterPinTextMode else LeavePinTextMode
                    else
                      _qbutton state.theme "Exit" LeavePinTextMode
                  else HH.text ""
                ]

             : ( if pinnedCount > 0 then
                  HH.div [ HP.style Style.pinnedScroll ]
                  $ pinScrollButton <$> Array.range 0 (pinnedCount - 1)
               else
                  HH.text "" )

             : if not state.pinnedTextMode then
                  pure $ HH.div_
                    $ wrapPinUnpin state modes.pinMode gconfig.render Pinned
                   <$> (checkScroll state.pinnedScrollTo $ Array.fromFoldable state.pinned)
               else
                  pure $ HH.div
                    [ HP.style $ Style.textEditBox
                    ]
                    [ HH.textarea
                      [ HP.style $ Style.textarea state.theme
                      , HP.value $ String.joinWith "\n" $ pinnedTextLine <$> Array.fromFoldable state.pinned
                      , HP.rows 15
                      , HP.cols 30
                      , HE.onFocusIn  $ const PauseListeningKeys
                      , HE.onFocusOut $ const ResumeListeningKeys
                      , HE.onBlur     $ const ResumeListeningKeys
                      ]
                    ]

        {- Fold // String Rep -}
        L.E L.FoldTreeOrEdit ->
          if not state.treeTextMode then

            let
              singleItemHeight = 13.5
              itemsInColumn = Int.floor $ lo.rect.size.height / singleItemHeight
            in HH.div
              [ HP.style $ Style.foldRepBox lo.rect.size
              , HE.onClick $ const EnterTreeTextMode
              ]
              $  mapWithIndex foldRepColumn
             <$> splitBy itemsInColumn
              $  foldRepLine
             <$> foldRepLines

          else

            HH.div
              [ HP.style $ Style.textEditBox
              ]
              [ HH.textarea
                [ HP.style $ Style.textarea state.theme
                , HP.value $ TreeConv.toString TreeConv.Indent SvgI._export state.tree
                , HP.rows 50
                , HP.cols 50
                , HE.onValueChange TryParse
                , HE.onFocusIn  $ const PauseListeningKeys
                , HE.onFocusOut $ const ResumeListeningKeys
                , HE.onBlur     $ const ResumeListeningKeys
                ]
              ]

        {- Export / JSON Rep -}
        L.E L.Export ->
          HH.div
            [ HP.style $ Style.jsonRepBox
            ]
            [ HH.textarea
              [ HP.style $ Style.textarea state.theme
              , HP.value $ case state.exportMode of
                    Json -> TreeConv.writeJSON state.tree
                    Text tmode -> TreeConv.toString tmode SvgI._export state.tree
                    Dot -> TreeConv.toDotText'
                              (TreeConv.dotConvertWithLabel
                                    (const $ SvgI._export >>> TreeConv.DotId) -- TODO
                                    (const $ SvgI._export)
                              )
                              state.tree
              , HP.readOnly true
              , HP.rows 10
              , HP.cols 50
              ]
            , HH.div
              []
               $  (\exp -> _qbutton state.theme (exportLabel exp) $ SwitchExport exp)
              <$> Set.toUnfoldable state.exports
            ]

        {- Zoom & Size -}
        L.E L.ZoomControls ->
          HH.div
            [ HP.style Style.zoomBox ]
            [ _qbutton state.theme "Reset zoom" ResetZoom
            , HH.span [ HP.style Style.zoomItem ] [ label $ "Zoom : " <> (String.take 6 $ show state.zoom) ]
            , HH.span [ HP.style Style.zoomItem ] [ label $ "Size : " <> show state.size.width <> "x" <> show state.size.height ]
            ]

        {- Keyboard hints -}
        L.E L.Hints ->
          HH.div
            [ HP.style $ Style.hintsBox state.theme ]
            $ HH.span [ HP.style Style.hintsLine ] <$> pure <$>
              (
              [ label "\"+\" to slightly zoom in"
              , label "\"-\" to slightly zoom out"
              , label "\"=\" to reset zoom"
              , label $ if Array.length state.history > 1 then "<Beckspace> to navigate one step back in history" else ""
              ]
              <>
              case state.selection of
                Just _ ->
                    [ label "<Escape> to cancel selection"
                    , label "<number> to select move to a next child"
                    , label "arrows to navigate tree up/down/right/left"
                    , label "<Enter> to navigate to selected node"
                    , label "<.> to pin selected node"
                    ]
                Nothing ->
                    [ label "<Space> to start navigating with keyboard"
                    , label "<.> to pin hovered node"
                    , if Path.depth state.focus > 0 then label "\"*\" or <Tab> to return to the root" else label ""
                    ]
              )

        L.S _ -> HH.text "" -- moveTo lo.rect.pos [ HS.text [] [ HH.text label ] ]

      -- renderIfEnabled element comp =
      --   if Set.member element state.elements then
      --     comp
      --   else
      --     HH.div [] []

      selectSize =
        case modes.previewMode of
          SvgTree.Component ->
            case state.preview of -- FIXME: xxx: merge the similar logic: `Preview` and `Selection` in one `State` field (but remember that we also need the `Selection` path for keyboard purposes only)
              Focused path -> sizeOf path
              LostFocus path ->
                case state.selection of
                  Just selPath -> sizeOf selPath
                  Nothing      -> sizeOf path
              None ->
                case state.selection of
                    Just selPath -> sizeOf selPath
                    Nothing      -> defaultSelectSize
          _ -> defaultSelectSize

      pinnedTextLine path =
        case Tree.value <$> Path.find path state.tree of
          Just value -> SvgI.pinnedLine path value
          Nothing -> "?"

      foldRepColumn colN =
        HH.div [ HP.style $ Style.foldRepColumn gconfig.geometry.foldColumnWidth colN ]

      fixFoldFocusPath (Path path) =
        case state.foldMode of
          All -> Path path
          OnlyFocus -> case state.focus of
            Path focusPath -> Path $ focusPath <> path

      foldRepLines
           =  map (lmap fixFoldFocusPath)
          <$> TreeConv.toPathLines (TreeConv.modeToF TreeConv.Indent)
           $  SvgI.foldLabel
          <$> case state.foldMode of
                All -> state.tree
                OnlyFocus ->
                    fromMaybe state.tree
                    $ Path.find state.focus state.tree

      foldRepLine (path /\ str) =
        HH.div
          [ HP.style
              $ Style.foldRepLine
              $ statusColor state.theme
              $ statusOf state path
          , HE.onClick $ flip FoldSelect path
          ]
          $  Array.replicate
                (Path.depth path)
                (HH.span
                  [ HP.style Style.foldRepIndent ]
                  [ HH.text "o" ]
                )
          <> [ HH.span [] [ HH.text str ] ]

  splitBy :: forall b. Int -> Array b -> Array (Array b)
  splitBy n =
    mapWithIndex (\idx a -> idx `div` n /\ a)
      >>> Array.groupBy (\a b -> Tuple.fst a == Tuple.fst b)
      >>> map NEA.toArray
      >>> map (map Tuple.snd)

  wrapPinUnpin :: State a -> SvgTree.NodeMode -> SvgTree.RenderConfig a -> PinUnpinMode_ -> Path -> HH.HTML _ (Action a)
  wrapPinUnpin state nodeMode config pupMode nodePath =
    HH.div
      [ HP.style Style.pinBox ]
      [ HH.div
        [ ]
        [ if not $ Set.member nodePath state.pinned then
            _qbutton state.theme "Pin" $ Pin nodePath
          else
            _qbutton state.theme "Unpin" $ UnPin nodePath
        , case pupMode of
            Pinned    -> renderPath state.theme SingleGo config state.tree nodePath
            Selection -> renderPath state.theme ReadOnly config state.tree nodePath
            Preview   -> renderPath state.theme ReadOnly config state.tree nodePath
        ]
      , case pupMode of
        Pinned    -> pinnedAt  state nodeMode nodePath
        Selection -> previewAt state nodeMode nodePath
        Preview   -> previewAt state nodeMode nodePath
      ]

  computeNum :: Array Int -> Int -> Int
  computeNum digits n =
    n + Tuple.snd (foldl (\(pow /\ sum) digit -> (pow * 10) /\ (digit * pow + sum)) (10 /\ 0) digits)

  updateFocus :: Path -> State a -> State a
  updateFocus newPath s =
    s
      { focus = newPath
      , history =
          if (s.focus /= newPath) && (Array.last s.history /= Just newPath)
            then Array.snoc s.history newPath
            else s.history
      }

  updateSelection :: Path -> State a -> State a
  updateSelection newPath = _ { selection = Just newPath, numKeyBuffer = [] }

  clearSelection :: State a -> State a
  clearSelection = _ { selection = Nothing, numKeyBuffer = [] }

  handleQuery :: forall q. Query q -> SvgTreeM m a (Maybe q)
  handleQuery = case _ of
    RequestFocus reply -> do
      s <- H.get
      pure $ Just $ reply s.focus
    RequestSelection reply -> do
      s <- H.get
      pure $ Just $ reply s.selection
    ChangeSelection path q -> do
      H.modify_ $ updateSelection path
      pure $ Just q
    ClearSelection q -> do
      H.modify_ $ clearSelection
      pure $ Just q
    ChangeFocus path q -> do
      H.modify_ $ updateFocus path
      pure $ Just q

  handleAction :: Action a -> SvgTreeM m a Unit
  handleAction = case _ of
    Initialize -> do
      document <- H.liftEffect $ document =<< window
      H.subscribe' \sid ->
          eventListener
            KET.keyup
            (HTMLDocument.toEventTarget document)
            (map (HandleKey sid) <<< KE.fromEvent)
      H.raise $ SelectionCleared
      H.raise $ FocusChanged Path.root
    Receive input ->
      H.modify_ $ receive input
    WheelChange { dy } ->
      H.modify_ (\state ->
        state
          { zoom
            = min scaleLimit.max
            $ max scaleLimit.min
            $ state.zoom + (dy * 0.1)
          }
      )
    HandleKey sid evt -> do
      state <- H.get
      when (not state.editingSomeText) $ do
        H.liftEffect $ E.preventDefault $ KE.toEvent evt
        -- H.liftEffect $ Console.log $ "key:" <> KE.key evt
        -- H.liftEffect $ Console.log $ "code:" <> KE.code evt
        handleKey $ KE.key evt
    ResetZoom ->
      H.modify_ _ { zoom = 1.0 }
    TryParse userInput -> do
      s <- H.get
      handleAction
        $ Receive
          { size : s.size
          , tree : TreeConv.fromString SvgI._import userInput <#> fromMaybe SvgI.default
          , elements : s.elements
          , exports : s.exports
          , theme : s.theme
          , depthLimit : s.depthLimit
          , childrenLimit : s.childrenLimit
          , mbFocus : Nothing
          , breadcrumbsAction : s.breadcrumbsAction
          , showChildrenCount : s.showChildrenCount
          , foldColumnWidth : s.foldColumnWidth
          }
    FocusOn path -> do
      H.modify_ $ updateFocus path
      H.raise $ FocusChanged path
    NodeClick path _ -> do
      H.modify_ $ updateFocus path
      H.raise $ FocusChanged path
    NodeOver path _ ->
      H.modify_ _ { preview = Focused path }
    NodeOut path _ ->
      H.modify_ _ { preview = LostFocus path }
    PreviewOver path ->
      H.modify_ _ { preview = Focused path }
    Pin path ->
      H.modify_ \s ->
        let nextPinned = s.pinned # Set.insert path
        in s
          { pinned = nextPinned
          , pinnedScrollTo = ensureInRange nextPinned s.pinnedScrollTo
          }
    UnPin path ->
      H.modify_ \s ->
        let nextPinned = s.pinned # Set.delete path
        in s
          { pinned = nextPinned
          , pinnedScrollTo = ensureInRange nextPinned s.pinnedScrollTo
          }
    PauseListeningKeys ->
      H.modify_ _ { editingSomeText = true }
    ResumeListeningKeys ->
      H.modify_ _ { editingSomeText = false }
    EnterTreeTextMode ->
      H.modify_ _ { treeTextMode = true }
    LeaveTreeTextMode ->
      H.modify_ _ { treeTextMode = false, pinnedTextMode = false, editingSomeText = false }
    EnterPinTextMode ->
      H.modify_ _ { pinnedTextMode = true }
    LeavePinTextMode ->
      H.modify_ _ { pinnedTextMode = false, treeTextMode = false, editingSomeText = false }
    SwitchExport emode ->
      H.modify_ _ { exportMode = emode }
    FoldSelect mevt path -> do
      H.liftEffect $ E.stopPropagation $ ME.toEvent mevt
      H.modify_ _ { selection = Just path }
      H.raise $ SelectionChanged path
    PinScroll n ->
      H.modify_ _ { pinnedScrollTo = Just n }
    ToggleFoldMode ->
      H.modify_ \s -> s { foldMode =
        case s.foldMode of
          OnlyFocus -> All
          All -> OnlyFocus
      }
    BreadcrumbsAction path ->
      H.raise $ BreadcrumbsActionTriggered path
    DoNothing -> pure unit

  handleKey key | key == "+" = H.modify_ \s -> s { zoom = s.zoom + 0.1 }
  handleKey key | key == "-" = H.modify_ \s -> s { zoom = s.zoom - 0.1 }
  handleKey key | key == "=" = H.modify_ _ { zoom = 1.0 }
  handleKey key | key == "." =
      H.modify_ \s ->
        s { pinned = s.pinned # Set.insert ( case s.preview of
              Focused previewPath -> previewPath
              _ -> case s.selection of
                Just selPath -> selPath
                Nothing -> s.focus
            )
        }
  handleKey key | key == " " = do
      s <- H.modify \s -> s { selection = Just s.focus }
      H.raise $ SelectionChanged s.focus
  handleKey key | key == "*" = do
      H.modify_ $ updateFocus Path.root
      H.raise $ FocusChanged Path.root
  handleKey key | String.toLower key == "tab" = do
      H.modify_ $ updateFocus Path.root
      H.raise $ FocusChanged Path.root
  handleKey key | String.toLower key == "escape" = do
      H.modify_ \s -> s
        { selection = Nothing
        , preview = None
        , treeTextMode = false
        , editingSomeText = false
        , numKeyBuffer = []
        }
      H.raise SelectionCleared
  handleKey key | String.toLower key == "backspace" = do
      s <- H.modify \s ->
        case s.history of
          [] -> s
          [ _ ] -> s
          _ ->
            let
              nextHistory = Array.dropEnd 1 s.history
            in s
              { history = nextHistory
              , focus = Array.last nextHistory # fromMaybe Path.root
              , numKeyBuffer = []
              }
      H.raise $ FocusChanged s.focus
  handleKey key | String.toLower key == "enter" = do
      s <- H.modify $ \s -> case s.selection of
              Just path ->
                updateFocus path $ clearSelection s
              Nothing -> s
      H.raise $ SelectionCleared
      H.raise $ FocusChanged s.focus
  -- TODO backspace : go to second last in the history
  -- TODO `up` in normal mode: move focus up
  handleKey key = do
    s <- H.get
    _whenJust s.selection \selPath -> do
      _whenJust (_isNumberKey key) \num ->
        _whenJust (Path.find selPath s.tree) \subTree ->
          let
            bufferedNums  = Array.length s.numKeyBuffer
            childrenCount = Array.length $ Tree.children subTree
            completeNum   = if bufferedNums > 0 then computeNum s.numKeyBuffer num else num
          in if (childrenCount <= Int.pow 10 (bufferedNums + 1)) && (completeNum < childrenCount) then
            let nextPath = Path.safeAdvance selPath completeNum s.tree
            in do
              H.put $ updateSelection nextPath $ s
              H.raise $ SelectionChanged nextPath
          else
            H.put $ s { numKeyBuffer = if bufferedNums < 3 then num : s.numKeyBuffer else [] }
      _whenJust (_isArrowKey key) $ \dir ->
          let nextPath = Path.advanceDir selPath dir s.tree
          in do
            if dir == Path.Up && selPath == s.focus then do
              H.put $ updateFocus nextPath $ updateSelection nextPath $ s
              H.raise $ FocusChanged nextPath
            else
              H.put $ updateSelection nextPath $ s
            H.raise $ SelectionChanged nextPath

  ensureInRange nextPinned curScrollTo =
    curScrollTo >>= \n ->
      if n >= 0 && n < Set.size nextPinned then Just n else Nothing


_whenJust :: forall a m. Applicative m => Maybe a -> (a -> m Unit) -> m Unit
_whenJust maybe f =
    case maybe of
        Just v -> f v
        Nothing -> pure unit


_isNumberKey :: String -> Maybe Int
_isNumberKey = case _ of
    "0" -> Just 0
    "1" -> Just 1
    "2" -> Just 2
    "3" -> Just 3
    "4" -> Just 4
    "5" -> Just 5
    "6" -> Just 6
    "7" -> Just 7
    "8" -> Just 8
    "9" -> Just 9
    _   -> Nothing


_isArrowKey :: String -> Maybe Path.Dir
_isArrowKey = case _ of
    "ArrowUp"    -> Just Path.Up
    "ArrowDown"  -> Just Path.Down
    "ArrowRight" -> Just Path.Right
    "ArrowLeft"  -> Just Path.Left
    _ -> Nothing


_qbutton :: forall p action. Style.Theme -> String -> action -> HH.HTML p action
_qbutton theme = _qbutton' $ Style.button theme


_qbutton' :: forall p action. String -> String -> action -> HH.HTML p action
_qbutton' style label action =
    HH.button
      [ HP.style style
      , HE.onClick $ const action
      ]
      [ HH.text label ]


_pathStepButtonRaw :: forall p a. Style.Theme -> Boolean -> String -> Maybe (Action a) -> HH.HTML p (Action a)
_pathStepButtonRaw theme isReadOnly label = case _ of
    Just action -> _qbutton' (Style.pathStep theme isReadOnly) label action
    Nothing ->
      HH.span
        [ HP.style (Style.pathStep theme isReadOnly) ]
        [ HH.text label ]


_pathRootButton :: forall a p. Style.Theme -> Boolean -> (Path -> a -> String) -> Tree a -> HH.HTML p (Action a)
_pathRootButton theme isReadOnly toLabel tree =
    let
      mbValueAt = tree # Path.find Path.root <#> Tree.value
      buttonLabel = maybe "*" (\val -> toLabel Path.root val <> " [*]") mbValueAt
    in _pathStepButtonRaw theme isReadOnly buttonLabel $ Just $ FocusOn Path.root


_pathReadOnlyRoot :: forall a p. Style.Theme -> (Path -> a -> String) -> Tree a -> HH.HTML p (Action a)
_pathReadOnlyRoot theme toLabel tree =
    let
      mbValueAt = tree # Path.find Path.root <#> Tree.value
      buttonLabel = maybe "*" (\val -> toLabel Path.root val <> " [*]") mbValueAt
    in _pathStepButtonRaw theme false buttonLabel Nothing


_pathStepButton :: forall a p. Style.Theme -> Boolean -> (Path -> a -> String) -> Tree a -> Path -> Int -> Int -> HH.HTML p (Action a)
_pathStepButton theme isReadOnly toLabel tree fullPath pStepIndex pValueAtDepth =
    -- pStepIndexis the index of the depth layer.
    -- pValueAtDepth is the position of the node at this level of depth
    let
      pathDepth = Path.depth fullPath
      isLast = pStepIndex == (pathDepth - 1)
      curPath = Path $ Array.take (pStepIndex + 1) $ Path.toArray fullPath  --Array.dropEnd (max 0 $ pathLen - pIndex - 1) fullPath
      mbValueAt = tree # Path.find curPath <#> Tree.value
      buttonLabel = maybe (show pValueAtDepth) (\val -> toLabel curPath val <> " [" <> show pValueAtDepth <> "]") mbValueAt
    in
    if not isReadOnly && not isLast
      then _pathStepButtonRaw theme false buttonLabel $ Just $ FocusOn curPath
      else _pathStepButtonRaw theme true  buttonLabel Nothing


renderPath :: forall p a. Style.Theme -> PathMode -> SvgTree.RenderConfig a -> Tree a -> Path -> HH.HTML p (Action a)
renderPath theme (Breadcrumbs { withAction }) config tree path =
  case Path.toArray path of
    [] ->
      HH.div [ HP.style Style.pathBox ] [ _pathReadOnlyRoot theme config.valueLabel tree ]
    pathArr ->
      HH.div
        [ HP.style Style.pathBox ]
        $ _pathRootButton theme false config.valueLabel tree
        : mapWithIndex (_pathStepButton theme false config.valueLabel tree path) pathArr
       <> (if withAction then [ _qbutton theme "^" $ BreadcrumbsAction path ] else [ ])
renderPath theme ReadOnly config tree path =
  HH.div
    [ HP.style Style.pathBox ]
    $ _pathRootButton theme true config.valueLabel tree
    : mapWithIndex (_pathStepButton theme true config.valueLabel tree path) (Path.toArray path)
renderPath theme SingleGo config tree path =
  HH.div
    [ HP.style Style.pathWithGo ]
    [ _qbutton theme "Go" $ FocusOn path
    , renderPath theme ReadOnly config tree path
    ]


_graphStatus :: forall a. State a -> SvgTree.GraphStatus
_graphStatus state =
  case state.selection of
    Just _ -> SvgTree.GKeysFocus
    Nothing ->
      case state.preview of
        Focused _   -> SvgTree.GHoverFocus
        LostFocus _ -> SvgTree.GGhostFocus
        None        -> SvgTree.GNormal


exportLabel :: ExportMode -> String
exportLabel = case _ of
  Json -> "JSON"
  Dot -> "DOT"
  Text TreeConv.Lines -> "Lines"
  Text TreeConv.Indent -> "Indent"
  Text TreeConv.Paths -> "Paths"
  Text TreeConv.Corners -> "Corners"
  Text TreeConv.Triangles -> "Triangles"
  Text TreeConv.Dashes -> "Dashes"


instance Eq ExportMode where
  eq Json Json = true
  eq Dot Dot = true
  eq (Text TreeConv.Lines)     (Text TreeConv.Lines) = true
  eq (Text TreeConv.Indent)    (Text TreeConv.Indent) = true
  eq (Text TreeConv.Paths)     (Text TreeConv.Paths) = true
  eq (Text TreeConv.Corners)   (Text TreeConv.Corners) = true
  eq (Text TreeConv.Triangles) (Text TreeConv.Triangles) = true
  eq (Text TreeConv.Dashes)    (Text TreeConv.Dashes) = true
  eq _ _ = false

instance Ord ExportMode where
  compare Json Json = EQ
  compare Dot Dot = EQ
  compare (Text TreeConv.Lines)     (Text TreeConv.Lines) = EQ
  compare (Text TreeConv.Indent)    (Text TreeConv.Indent) = EQ
  compare (Text TreeConv.Paths)     (Text TreeConv.Paths) = EQ
  compare (Text TreeConv.Corners)   (Text TreeConv.Corners) = EQ
  compare (Text TreeConv.Triangles) (Text TreeConv.Triangles) = EQ
  compare (Text TreeConv.Dashes)    (Text TreeConv.Dashes) = EQ
  compare Json _ = GT
  compare Dot _ = GT
  compare (Text TreeConv.Lines)     _ = GT
  compare (Text TreeConv.Indent)    _ = GT
  compare (Text TreeConv.Paths)     _ = GT
  compare (Text TreeConv.Corners)   _ = GT
  compare (Text TreeConv.Triangles) _ = GT
  compare (Text TreeConv.Dashes)    _ = GT
