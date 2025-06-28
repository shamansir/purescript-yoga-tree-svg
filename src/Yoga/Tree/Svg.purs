module Yoga.Tree.Svg
  ( NodeComponent, Element(..)
  , component, component_
  , allElements
  ) where

import Prelude

import Effect.Class (class MonadEffect)
import Effect.Console (log) as Console

import Data.Array ((:))
import Data.Array (take, fromFoldable, dropEnd, length, snoc, last, reverse) as Array
import Data.FunctorWithIndex (mapWithIndex)
import Data.Graph (Graph)
import Data.Graph (fromMap, toMap) as Graph
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Set (Set)
import Data.Set (empty, insert, delete, isEmpty, member, fromFoldable) as Set
import Data.String (take, toLower) as String
import Data.Tuple.Nested ((/\))

import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Query.Event (eventListener)
import Halogen.Svg.Attributes as HSA
import Halogen.Svg.Elements as HS

import Web.HTML (window, HTMLDocument)
import Web.HTML.HTMLDocument as HTMLDocument
import Web.HTML.Window (document, history)
import Web.UIEvent.KeyboardEvent as KE
import Web.UIEvent.KeyboardEvent.EventTypes as KET
import Web.UIEvent.WheelEvent as WE
import Web.Event.Event as E

import Yoga.Tree (Tree)
import Yoga.Tree.Extended (value, children) as Tree
import Yoga.Tree.Extended.Graph (toGraph') as Tree
import Yoga.Tree.Extended.Path (Path(..))
import Yoga.Tree.Extended.Path (find, root, toArray, depth, advance, safeAdvance, Dir(..), advanceDir, isNextFor) as Path
import Yoga.Tree.Extended.Convert as TreeConv
import Yoga.Tree.Svg.Render (WithStatus)
import Yoga.Tree.Svg.Render as SvgTree
import Yoga.Tree.Svg.SvgItem (class IsSvgTreeItem)
import Yoga.Tree.Svg.SvgItem (toText, fromText, default) as SvgI
import Yoga.Tree.Svg.Style as Style

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
    | ResetZoom
    | Pin Path
    | UnPin Path
    | TryParse String
    | PauseListeningKeys
    | ResumeListeningKeys


data PathMode
    = Breadcrumbs
    | ReadOnly
    | SingleGo


-- data PathItemMode
--     = PIReadOnly
--     | PINav


data Element
    = Preview
    | History
    | Pinned
    | TextEdit
    | JsonOutput
    | Hints
    | ZoomControls
    | FoldTree


derive instance Eq Element
derive instance Ord Element


allElements :: Set Element
allElements = Set.fromFoldable [ Preview, History, Pinned, TextEdit, JsonOutput, Hints, ZoomControls, FoldTree ]


type Input a =
    { tree :: Tree a
    , size :: { width :: Number, height :: Number }
    , elements :: Set Element
    , mode :: Style.Mode
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
    , editingTreeText :: Boolean
    , elements :: Set Element
    , mode :: Style.Mode
    }


type NodeComponent m a = SvgTree.NodeComponent m a


component :: forall a query output m. MonadEffect m => IsSvgTreeItem a => SvgTree.Modes -> SvgTree.Config a -> H.Component query (Input a) output m
component modes config = component' modes config Nothing


component_ :: forall a query output m. MonadEffect m => IsSvgTreeItem a => SvgTree.Modes -> SvgTree.Config a -> NodeComponent m a -> H.Component query (Input a) output m
component_ modes config childComp = component' modes config (Just childComp)


component' :: forall a query output m. MonadEffect m => IsSvgTreeItem a => SvgTree.Modes -> SvgTree.Config a -> Maybe (NodeComponent m a) -> H.Component query (Input a) output m
component' modes config mbChildComp =
  H.mkComponent
    { initialState
    , render
    , eval: H.mkEval $ H.defaultEval
        { handleAction = handleAction
        , receive = Just <<< Receive
        , initialize = Just Initialize
        }
    }
  where

  geometry :: SvgTree.Geometry
  geometry =
    { scaleFactor : 10.0
    , baseDistance : 30.0
    , valueRadius : 5.0
    , scaleLimit : { min : 0.2, max : 50.0 }
    }

  initialState :: Input a -> State a
  initialState { tree, size, elements, mode } =
    { tree
    , focus : Path.root
    , history : [ Path.root ]
    , zoom : 1.0
    , size
    , preview : None
    , pinned : Set.empty
    , selection : Nothing
    , editingTreeText : false
    , elements
    , mode
    }

  receive :: Input a -> State a -> State a
  receive { tree, size } =
    _ { size = size, tree = tree }

  events :: SvgTree.Events (Action a) a
  events =
    { valueClick : NodeClick
    , valueOver  : NodeOver
    , valueOut   : NodeOut
    }

  previewAt tree previewPath =
    case Path.find previewPath tree of
      Just previewNode ->
        SvgTree.renderPreview' modes.previewMode geometry config mbChildComp SvgTree.Normal previewPath $ Tree.value previewNode
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

  render :: State a -> _
  render state =
    HH.div
      [ HP.style $ Style.component state.size.width state.size.height ]

      {- Graph component -}
      [ HH.div
        [ HP.style Style.graph ]
        $ pure
        $ HS.svg
          [ HSA.width state.size.width
          , HSA.height state.size.height
          , HE.onWheel \wevt -> WheelChange
              { dx : WE.deltaX wevt
              , dy : WE.deltaY wevt
              }
          ]
          $ pure
          $ HS.g
            [ HSA.transform [ HSA.Translate (state.size.width / 3.0) (state.size.height * 0.3) ] ]
            $ SvgTree.renderGraph' (_graphStatus state) modes (geometry { scaleFactor = state.zoom * 5.0 }) config mbChildComp events
            -- FIXME: passing `state.focus` is needed only because else we would first fill already focused `Tree` with `Paths` when converting it to `Graph`
            $ injectNodeStatuses state
            $ Tree.toGraph' state.focus
            $ fromMaybe state.tree
            $ Path.find state.focus state.tree

      {- Zoom & Size -}
      , renderIfEnabled ZoomControls $ HH.div
        [ HP.style Style.zoomBox ]
        [ _qbutton "Reset zoom" ResetZoom
        , HH.span [ HP.style Style.zoomItem ] [ HH.text $ "Zoom : " <> (String.take 6 $ show state.zoom) ]
        , HH.span [ HP.style Style.zoomItem ] [ HH.text $ "Size : " <> show state.size.width <> "x" <> show state.size.height ]
        ]

      {- Keyboard info -}

      , renderIfEnabled Hints $ HH.div
        [ HP.style Style.hintsBox ]
        $ HH.span [ HP.style Style.hintsLine ] <$> pure <$>
          (
          [ HH.text "\"+\" to slightly zoom in"
          , HH.text "\"-\" to slightly zoom out"
          , HH.text "\"=\" to reset zoom"
          ]
          <>
          case state.selection of
            Just _ ->
                [ HH.text "<Escape> to cancel selection"
                , HH.text "<number> to select move to a next child"
                , HH.text "arrows to navigate tree up/down/right/left"
                , HH.text "<Enter> to navigate to selected node"
                , HH.text "<.> to pin selected node"
                ]
            Nothing ->
                [ HH.text "<Space> to start navigating with keyboard"
                , HH.text "<.> to pin hovered node"
                , if Path.depth state.focus > 0 then HH.text "\"*\" or <Tab> to return to the root" else HH.text ""
                ]
          )

      {- Path Breadcrumbs -}
      , HH.div
          -- [ HP.style "position: absolute; right: 0; top: 20px;" ]
          [ HP.style $ case state.selection of
            Just _  -> Style.breadcrumbsWithSelection
            Nothing -> Style.breadcrumbs
          ]
          [ HH.text "Location: "
          , renderPath Breadcrumbs config state.tree state.focus
          , case state.selection of
              Just selPath ->
                HH.div
                  []
                  [ HH.text "Selection:"
                  , renderPath Breadcrumbs config state.tree selPath
                  ]
              Nothing ->
                HH.text ""
          ]

      {- Current Preview(s) -}
      , renderIfEnabled Preview $ HH.div
          [ HP.style $ case state.preview of
            Focused _   -> Style.previewFocused
            LostFocus _ -> Style.previewBlurred
            None -> Style.previewNone
          ]
          $ pure
          $ case state.preview of
              Focused previewPath ->
                if not $ Set.member previewPath state.pinned then
                  wrapPinUnpin config false state.pinned state.tree previewPath
                else HH.text ""
              LostFocus previewPath ->
                if not $ Set.member previewPath state.pinned then
                  wrapPinUnpin config false state.pinned state.tree previewPath
                else HH.text ""
              None ->
                HH.text ""

      {- Current Pinned -}
      , renderIfEnabled Pinned $ HH.div
          [ HP.style $ Style.pinnedBox
          ]
          $ wrapPinUnpin config true state.pinned state.tree
          <$> Array.fromFoldable state.pinned

      {- History -}
      , renderIfEnabled History $ HH.div
          [ HP.style $ Style.historyBox
          ]
          $ (HH.div [] <<< pure <<< renderPath SingleGo config state.tree)
          <$> Array.reverse state.history

      {- String Rep -}
      , renderIfEnabled TextEdit $ HH.div
          [ HP.style $ Style.textEditBox
          ]
          [ HH.textarea
            [ HP.style Style.textarea
            , HP.value $ TreeConv.toString TreeConv.Indent SvgI.toText state.tree
            , HP.rows 50
            , HP.cols 50
            , HE.onValueChange TryParse
            , HE.onFocusIn $ const PauseListeningKeys
            , HE.onFocusOut $ const ResumeListeningKeys
            ]
          ]

      {- JSON Rep -}
      , renderIfEnabled JsonOutput $ HH.div
          [ HP.style $ Style.jsonRepBox
          ]
          [ HH.textarea
            [ HP.style Style.textarea
            , HP.value $ TreeConv.writeJSON state.tree
            , HP.readOnly true
            , HP.rows 10
            , HP.cols 50
            ]
          ]

      ]
    where
      renderIfEnabled element comp =
        if Set.member element state.elements then
          comp
        else
          HH.div [] [ ]

  wrapPinUnpin config allowGo pinned tree nodePath =
    HH.div
      [ HP.style Style.pinBox ]
      [ HH.div
        [ ]
        [ if not $ Set.member nodePath pinned then
            _qbutton "Pin" $ Pin nodePath
          else
            _qbutton "Unpin" $ UnPin nodePath
        , if allowGo then
            renderPath SingleGo config tree nodePath
          else
            renderPath ReadOnly config tree nodePath
        ]
      , previewAt tree nodePath
      ]

  updateFocus newPath s =
    s
      { focus = newPath
      , history =
          if (s.focus /= newPath) && (Array.last s.history /= Just newPath)
            then Array.snoc s.history newPath
            else s.history
      }

  handleAction = case _ of
    Initialize -> do
      document <- H.liftEffect $ document =<< window
      H.subscribe' \sid ->
          eventListener
            KET.keyup
            (HTMLDocument.toEventTarget document)
            (map (HandleKey sid) <<< KE.fromEvent)
    Receive input ->
      H.modify_ $ receive input
    WheelChange { dy } ->
      H.modify_ (\state ->
        state
          { zoom
            = min geometry.scaleLimit.max
            $ max geometry.scaleLimit.min
            $ state.zoom + (dy * 0.1)
          }
      )
    HandleKey sid evt -> do
      state <- H.get
      when (not state.editingTreeText) $ do
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
          , tree : TreeConv.fromString SvgI.fromText userInput <#> fromMaybe SvgI.default
          , elements : s.elements
          , mode : s.mode
          }
    FocusOn path ->
      H.modify_ $ updateFocus path
    NodeClick path _ ->
      H.modify_ $ updateFocus path
    NodeOver path _ ->
      H.modify_ _ { preview = Focused path }
    NodeOut path _ ->
      H.modify_ _ { preview = LostFocus path }
    Pin path ->
      H.modify_ \s -> s { pinned = s.pinned # Set.insert path }
    UnPin path ->
      H.modify_ \s -> s { pinned = s.pinned # Set.delete path }
    PauseListeningKeys ->
      H.modify_ _ { editingTreeText = true }
    ResumeListeningKeys ->
      H.modify_ _ { editingTreeText = false }

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
  handleKey key | key == " " =
      H.modify_ \s ->
        s { selection = Just s.focus }
  handleKey key | key == "*" = H.modify_ $ updateFocus Path.root
  handleKey key | String.toLower key == "tab" = H.modify_ $ updateFocus Path.root
  handleKey key | String.toLower key == "escape" =
      H.modify_ \s -> s { selection = Nothing }
  handleKey key | String.toLower key == "enter" = do
      s <- H.get
      H.put $ case s.selection of
              Just path ->
                updateFocus path $ s { selection = Nothing }
              Nothing -> s
  -- TODO backspace : go to second last in the history
  -- TODO `up` in normal mode: move focus up
  handleKey key = do
    s <- H.get
    _whenJust s.selection \selPath -> do
      _whenJust (_isNumberKey key) \num ->
        _whenJust (Path.find selPath s.tree) \subTree ->
          if num < Array.length (Tree.children subTree) then
            H.put $ s { selection = Just $ Path.safeAdvance selPath num s.tree }
          else pure unit
      _whenJust (_isArrowKey key) $ \dir ->
          let nextPath = Path.advanceDir selPath dir s.tree
          in
            if dir == Path.Up && selPath == s.focus then
              H.put $ updateFocus nextPath $ s { selection = Just nextPath }
            else
              H.put $ s { selection = Just nextPath }

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


_qbutton :: forall p action. String -> action -> HH.HTML p action
_qbutton = _qbutton' Style.button

_qbutton' :: forall p action. String -> String -> action -> HH.HTML p action
_qbutton' style label action =
    HH.button
      [ HP.style style
      , HE.onClick $ const action
      ]
      [ HH.text label ]


_pathStepButtonRaw :: forall p a. Boolean -> String -> Maybe (Action a) -> HH.HTML p (Action a)
_pathStepButtonRaw isReadOnly label = case _ of
    Just action -> _qbutton' (Style.pathStep isReadOnly) label action
    Nothing ->
      HH.span
        [ HP.style (Style.pathStep isReadOnly) ]
        [ HH.text label ]


_pathRootButton :: forall a p. Boolean -> (Path -> a -> String) -> Tree a -> HH.HTML p (Action a)
_pathRootButton isReadOnly toLabel tree =
    let
      mbValueAt = tree # Path.find Path.root <#> Tree.value
      buttonLabel = maybe "*" (\val -> toLabel Path.root val <> " [*]") mbValueAt
    in _pathStepButtonRaw isReadOnly buttonLabel $ Just $ FocusOn Path.root


_pathStepButton :: forall a p. Boolean -> (Path -> a -> String) -> Tree a -> Path -> Int -> Int -> HH.HTML p (Action a)
_pathStepButton isReadOnly toLabel tree fullPath pStepIndex pValueAtDepth =
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
      then _pathStepButtonRaw false buttonLabel $ Just $ FocusOn curPath
      else _pathStepButtonRaw true  buttonLabel Nothing


renderPath :: forall p a. PathMode -> SvgTree.Config a -> Tree a -> Path -> HH.HTML p (Action a)
renderPath Breadcrumbs config tree path =
  case Path.toArray path of
    [] ->
      HH.div [ HP.style Style.pathBox ] [ _pathStepButtonRaw false "*" Nothing ]
    pathArr ->
      HH.div
        [ HP.style Style.pathBox ]
        $ _pathRootButton false config.valueLabel tree
        : mapWithIndex (_pathStepButton false config.valueLabel tree path) pathArr
renderPath ReadOnly config tree path =
  HH.div
    [ HP.style Style.pathBox ]
    $ _pathRootButton true config.valueLabel tree
    : mapWithIndex (_pathStepButton true config.valueLabel tree path) (Path.toArray path)
renderPath SingleGo config tree path =
  HH.div
    [ HP.style Style.pathWithGo ]
    [ _qbutton "Go" $ FocusOn path
    , renderPath ReadOnly config tree path
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