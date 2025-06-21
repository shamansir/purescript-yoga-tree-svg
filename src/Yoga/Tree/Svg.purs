module Yoga.Tree.Svg
  ( NodeComponent
  , component, component_
  ) where

import Prelude

-- import Debug as Debug


import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Array ((:))
import Data.Array (fromFoldable, dropEnd, length) as Array
import Data.FunctorWithIndex (mapWithIndex)
import Data.String (take) as String
import Data.Set (Set)
import Data.Set (empty, insert, delete, isEmpty, member) as Set

import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.HTML.Events as HE
import Halogen.Svg.Attributes as HSA
import Halogen.Svg.Elements as HS

import Web.UIEvent.WheelEvent as Wheel

import Yoga.Tree (Tree)
import Yoga.Tree.Extended (value) as Tree
import Yoga.Tree.Extended.Path (Path)
import Yoga.Tree.Extended.Path (Path(..), find, root, toArray) as Path
import Yoga.Tree.Extended.Graph (toGraph') as Tree
import Yoga.Tree.Svg.Render as SvgTree
-- import Yoga.Tree.Zipper (Path)


data Action a
    = Receive (Input a)
    | WheelChange { dx :: Number, dy :: Number }
    | FocusOn Path
    | NodeClick Path a
    | NodeOver  Path a
    | NodeOut   Path a
    | ResetZoom
    | Pin Path
    | UnPin Path
    -- | Advance Int
    -- | GoUp


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
    , prevFocus :: Maybe Path
    , preview :: Preview
    , pinned :: Set Path
    , zoom :: Number
    , size :: { width :: Number, height :: Number }
    }


type NodeComponent m a = SvgTree.NodeComponent m a


component :: forall a query output m. Ord a => SvgTree.Modes -> SvgTree.Config a -> H.Component query (Input a) output m
component modes config = component' modes config Nothing


component_ :: forall a query output m. Ord a => SvgTree.Modes -> SvgTree.Config a -> NodeComponent m a -> H.Component query (Input a) output m
component_ modes config childComp = component' modes config (Just childComp)


component' :: forall a query output m. Ord a => SvgTree.Modes -> SvgTree.Config a -> Maybe (NodeComponent m a) -> H.Component query (Input a) output m
component' modes config mbChildComp =
  H.mkComponent
    { initialState
    , render
    , eval: H.mkEval $ H.defaultEval
        { handleAction = handleAction
        , receive = Just <<< Receive
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
    { tree, focus : Path.root, prevFocus : Nothing, zoom : 1.0, size, preview : None, pinned : Set.empty }

  receive :: Input a -> State a -> State a
  receive { tree, size } =
    _ { size = size, tree = tree }

  events :: SvgTree.Events (Action a) a
  events =
    { valueClick : NodeClick
    , valueOver  : NodeOver
    , valueOut   : NodeOut
    }

  rootButton :: (Path -> a -> String) -> Tree a -> _
  rootButton toLabel tree =
    let
      mbValueAt = tree # Path.find Path.root <#> Tree.value
      buttonLabel = maybe "*" (\val -> toLabel Path.root val <> " [*]") mbValueAt
    in qbutton buttonLabel $ FocusOn Path.root

  pathButton :: (Path -> a -> String) -> Tree a -> Array Int -> Int -> Int -> _
  pathButton toLabel tree fullPath pIndex pValue =
    let
      pathLen = Array.length fullPath
      isLast = pIndex == (pathLen - 1)
      curPath = Path.Path $ Array.dropEnd (max 0 $ pathLen - pIndex - 1) fullPath
      mbValueAt = tree # Path.find curPath <#> Tree.value
      buttonLabel = maybe (show pValue) (\val -> toLabel curPath val <> " [" <> show pValue <> "]") mbValueAt
    in
    if not isLast
      then qbutton buttonLabel $ FocusOn curPath
      else HH.text buttonLabel

  previewAt tree previewPath =
    case Path.find previewPath tree of
      Just previewNode ->
        SvgTree.renderPreview' modes.previewMode config mbChildComp previewPath $ Tree.value previewNode
      Nothing ->
        HH.text "?"

  qbutton label action =
    HH.button
      [ HP.style "cursor: pointer; pointer-events: all;"
      , HE.onClick $ const action
      ]
      [ HH.text label ]

  render :: State a -> _
  render state =
    HH.div
      [ HP.style "oveflow: hidden;" ]

      {- Graph component -}
      [ HH.div
        [ HP.style "position: absolute; left: 0; top: 0;" ]
        $ pure
        $ HS.svg
          [ HSA.width state.size.width
          , HSA.height state.size.height
          , HE.onWheel \wevt -> WheelChange
              { dx : Wheel.deltaX wevt
              , dy : Wheel.deltaY wevt
              }
          ]
          $ pure
          $ HS.g
            [ HSA.transform [ HSA.Translate 350.0 350.0 ] ]
            $ SvgTree.renderGraph' modes (geometry { scaleFactor = state.zoom * 5.0 }) config mbChildComp events
            -- FIXME: passing `state.focus` is needed only because else we would first fill already focused `Tree` with `Paths` when converting it to `Graph`
            $ Tree.toGraph' state.focus
            $ fromMaybe state.tree
            $ Path.find state.focus state.tree

      {- Zoom & Size -}
      , HH.div
        [ HP.style "position: absolute; right: 0; top: 0;" ]
        [ qbutton "ResetZoom" ResetZoom
        , HH.text $ "Zoom : " <> (String.take 6 $ show state.zoom)
        , HH.text $ "Size : " <> show state.size.width <> "x" <> show state.size.height
        ]

      {- Path Breadcrumbs -}
      , HH.div
          [ HP.style "position: absolute; right: 0; top: 20px;" ]
          [ case state.prevFocus of
            Just prevFocus ->
              qbutton "Back" $ FocusOn prevFocus
            Nothing -> HH.text ""
          , case Path.toArray state.focus of
              [] -> HH.text "*"
              path ->
                HH.div
                  []
                  $ rootButton config.valueLabel state.tree
                  : mapWithIndex (pathButton config.valueLabel state.tree path) path
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
                  wrapPinUnpin false state.pinned state.tree previewPath
                else HH.text ""
              LostFocus previewPath ->
                if not $ Set.member previewPath state.pinned then
                  wrapPinUnpin false state.pinned state.tree previewPath
                else HH.text ""
              None ->
                HH.text ""

      {- Current Pinned -}
      , HH.div
          [ HP.style $ "position: absolute; right: 0; top: 200px;"
          ]
          $ wrapPinUnpin true state.pinned state.tree
          <$> Array.fromFoldable state.pinned
      ]

  wrapPinUnpin allowGo pinned tree nodePath =
    HH.div
      []
      [ if not $ Set.member nodePath pinned then
          qbutton "Pin" $ Pin nodePath
        else
          qbutton "Unpin" $ UnPin nodePath
      , HH.text $ show nodePath
      , if allowGo then
          qbutton "Go" $ FocusOn nodePath
        else HH.text ""
      , previewAt tree nodePath
      ]

  updateFocus newPath s =
    s { focus = newPath, prevFocus = if s.focus /= newPath then Just s.focus else s.prevFocus }

  handleAction = case _ of
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