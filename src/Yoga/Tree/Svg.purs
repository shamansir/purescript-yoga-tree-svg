module Yoga.Tree.Svg
  ( NodeComponent
  , component, component_
  ) where

import Prelude

import Effect.Class (class MonadEffect)
import Effect.Console (log) as Console

import Data.Array ((:))
import Data.Array (take, fromFoldable, dropEnd, length, snoc, last, reverse) as Array
import Data.FunctorWithIndex (mapWithIndex)
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Set (Set)
import Data.Set (empty, insert, delete, isEmpty, member) as Set
import Data.String (take, toLower) as String
import Data.Tuple.Nested ((/\))
import Data.Graph (Graph)
import Data.Graph (fromMap, toMap) as Graph

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
import Web.UIEvent.WheelEvent as WE
import Web.UIEvent.KeyboardEvent as KE
import Web.UIEvent.KeyboardEvent.EventTypes as KET

import Yoga.Tree (Tree)
import Yoga.Tree.Extended (value) as Tree
import Yoga.Tree.Extended.Graph (toGraph') as Tree
import Yoga.Tree.Extended.Path (Path(..))
import Yoga.Tree.Extended.Path (find, root, toArray, depth, advance) as Path
import Yoga.Tree.Svg.Render (WithStatus)
import Yoga.Tree.Svg.Render as SvgTree

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
    -- | Advance Int
    -- | GoUp


data PathMode
    = Breadcrumbs
    | ReadOnly
    | SingleGo


type Input a =
    { tree :: Tree a
    , size :: { width :: Number, height :: Number }
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
    }


type NodeComponent m a = SvgTree.NodeComponent m a


component :: forall a query output m. MonadEffect m => SvgTree.Modes -> SvgTree.Config a -> H.Component query (Input a) output m
component modes config = component' modes config Nothing


component_ :: forall a query output m. MonadEffect m => SvgTree.Modes -> SvgTree.Config a -> NodeComponent m a -> H.Component query (Input a) output m
component_ modes config childComp = component' modes config (Just childComp)


component' :: forall a query output m. MonadEffect m => SvgTree.Modes -> SvgTree.Config a -> Maybe (NodeComponent m a) -> H.Component query (Input a) output m
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
    , baseRadius : 10.0
    , scaleLimit : { min : 0.2, max : 50.0 }
    }

  initialState :: Input a -> State a
  initialState { tree, size } =
    { tree
    , focus : Path.root
    , history : [ Path.root ]
    , zoom : 1.0
    , size
    , preview : None
    , pinned : Set.empty 
    , selection : Nothing 
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
        SvgTree.renderPreview' modes.previewMode config mbChildComp SvgTree.Normal previewPath $ Tree.value previewNode
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
              if selPath == path then SvgTree.Selected else checkPreview
            Nothing ->
              checkPreview
    in checkSelection

  injectStatuses :: State a -> Graph Path a -> Graph Path (WithStatus a)
  injectStatuses state = 
      Graph.toMap 
      >>> mapWithIndex (\path (a /\ xs) -> (statusOf state path /\ a) /\ xs) 
      >>> Graph.fromMap
  
  render :: State a -> _
  render state =
    HH.div
      [ HP.style "oveflow: hidden; user-select: none;" ]

      {- Graph component -}
      [ HH.div
        [ HP.style "position: absolute; left: 0; top: 0;" ]
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
            [ HSA.transform [ HSA.Translate 350.0 350.0 ] ]
            $ SvgTree.renderGraph' modes (geometry { scaleFactor = state.zoom * 5.0 }) config mbChildComp events
            -- FIXME: passing `state.focus` is needed only because else we would first fill already focused `Tree` with `Paths` when converting it to `Graph`
            $ injectStatuses state
            $ Tree.toGraph' state.focus
            $ fromMaybe state.tree
            $ Path.find state.focus state.tree

      {- Zoom & Size -}
      , HH.div
        [ HP.style "position: absolute; right: 0; top: 0; user-select: none;" ]
        [ _qbutton "Reset zoom" ResetZoom
        , HH.span [ HP.style _infoStyle ] [ HH.text $ "Zoom : " <> (String.take 6 $ show state.zoom) ]
        , HH.span [ HP.style _infoStyle ] [ HH.text $ "Size : " <> show state.size.width <> "x" <> show state.size.height ]
        ]

      {- Keyboard info -}

      , HH.div 
        []
        $ HH.span [ HP.style _infoBlStyle ] <$> pure <$>
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
                , HH.text "<Space> to pin selected node"
                ]
            Nothing -> 
                [ HH.text "<Space> to start navigating with keyboard" 
                ]
          )

      {- Path Breadcrumbs -}
      , HH.div
          -- [ HP.style "position: absolute; right: 0; top: 20px;" ]
          [ HP.style $ case state.selection of 
            Just _ -> "position: absolute; left: 0; top: 200px;"
            Nothing -> "position: absolute; left: 0; top: 120px;"
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
      , HH.div
          [ HP.style $ "position: absolute; right: 0; top: 100px; " <> case state.preview of
            Focused _ -> "opacity: 1.0;"
            LostFocus _ -> "opacity: 0.6;"
            None -> ""
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
      , HH.div
          [ HP.style $ "position: absolute; right: 0; top: 200px;"
          ]
          $ wrapPinUnpin config true state.pinned state.tree
          <$> Array.fromFoldable state.pinned

      {- History -}
      , HH.div
          [ HP.style $ "position: absolute; right: 0; top: 600px; user-select: none;"
          ]
          $ (HH.div [] <<< pure <<< renderPath SingleGo config state.tree)
          <$> Array.reverse state.history

      ]

  wrapPinUnpin config allowGo pinned tree nodePath =
    HH.div
      []
      [ if not $ Set.member nodePath pinned then
          _qbutton "Pin" $ Pin nodePath
        else
          _qbutton "Unpin" $ UnPin nodePath
      , if allowGo then
          renderPath SingleGo config tree nodePath
        else
          renderPath ReadOnly config tree nodePath
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
      H.liftEffect $ Console.log $ "key:" <> KE.key evt 
      H.liftEffect $ Console.log $ "code:" <> KE.code evt
      handleKey $ KE.key evt
    ResetZoom ->
      H.modify_ _ { zoom = 1.0 }
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

  handleKey key | key == "+" = H.modify_ \s -> s { zoom = s.zoom + 0.1 }
  handleKey key | key == "-" = H.modify_ \s -> s { zoom = s.zoom - 0.1 }
  handleKey key | key == "=" = H.modify_ _ { zoom = 1.0 }
  handleKey key | key == " " = 
      H.modify_ \s -> 
        case s.selection of 
            Just path -> 
              s { pinned = s.pinned # Set.insert path }
            Nothing -> 
              s { selection = Just s.focus }
  handleKey key | String.toLower key == "escape" = H.modify_ \s -> s { selection = Nothing }
  handleKey key = do 
    s <- H.get 
    case s.selection of 
        Just selPath ->
          case _isNumberKey key of 
             Just num -> 
               H.modify_ \s -> s { selection = Just (selPath # Path.advance num) }
             Nothing ->
               case _isArrowKey key of 
                 Just dir -> 
                   pure unit
                 Nothing -> 
                   pure unit
        Nothing ->  
          pure unit  

data Dir 
  = Up 
  | Down 
  | Right 
  | Left

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


_isArrowKey :: String -> Maybe Dir 
_isArrowKey = case _ of 
    "ArrowUp" -> Just Up
    "ArrowDown" -> Just Down
    "ArrowRight" -> Just Right
    "ArrowLeft" -> Just Left
    _ -> Nothing


_qbutton :: forall p action. String -> action -> HH.HTML p action
_qbutton = _qbutton' _buttonStyle   

_qbutton' :: forall p action. String -> String -> action -> HH.HTML p action
_qbutton' style label action =
    HH.button
      [ HP.style style
      , HE.onClick $ const action
      ]
      [ HH.text label ]


_buttonStyle   = "cursor: pointer; pointer-events: all; padding: 2px 5px; margin: 0px 2px; border-radius: 5px; border: 1px solid black; font-family: sans-serif; font-size: 11px; user-select: none;" :: String
_pathStepStyle = "cursor: pointer; pointer-events: all; padding: 2px 5px; margin: 0px 2px; border-radius: 5px; border: 1px solid blue;  font-family: sans-serif; font-size: 11px; user-select: none;" :: String 
_infoStyle     = "padding: 2px 5px;" :: String 
_infoBlStyle   = "padding: 2px 5px; display: block;" :: String 


_pathStepButtonRaw :: forall p a. String -> Maybe (Action a) -> HH.HTML p (Action a)
_pathStepButtonRaw label = case _ of 
    Just action -> _qbutton' _pathStepStyle label action
    Nothing -> 
      HH.span
        [ HP.style _pathStepStyle ]
        [ HH.text label ]


_pathRootButton :: forall a p. (Path -> a -> String) -> Tree a -> HH.HTML p (Action a)
_pathRootButton toLabel tree =
    let
      mbValueAt = tree # Path.find Path.root <#> Tree.value
      buttonLabel = maybe "*" (\val -> toLabel Path.root val <> " [*]") mbValueAt
    in _pathStepButtonRaw buttonLabel $ Just $ FocusOn Path.root


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
      then _pathStepButtonRaw buttonLabel $ Just $ FocusOn curPath
      else _pathStepButtonRaw buttonLabel Nothing
      

renderPath :: forall p a. PathMode -> SvgTree.Config a -> Tree a -> Path -> HH.HTML p (Action a)
renderPath Breadcrumbs config tree path =
  case Path.toArray path of
    [] -> 
      HH.div [] [ _pathStepButtonRaw "*" Nothing ]
    pathArr ->
      HH.div
        []
        $ _pathRootButton config.valueLabel tree
        : mapWithIndex (_pathStepButton false config.valueLabel tree path) pathArr
renderPath ReadOnly config tree path =
  HH.div 
    [] 
    $ _pathRootButton config.valueLabel tree
    : mapWithIndex (_pathStepButton true config.valueLabel tree path) (Path.toArray path)
renderPath SingleGo config tree path =
  HH.div  
    []
    [ renderPath ReadOnly config tree path
    , _qbutton "Go" $ FocusOn path 
    ]
