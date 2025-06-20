module Yoga.Tree.Svg.Component.Tree where

import Prelude

-- import Debug as Debug

import Type.Proxy (Proxy(..))

import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Foldable (foldl, foldr)
import Data.Traversable (traverse)
import Data.Tuple (uncurry)
import Data.Tuple (fst, snd) as Tuple
import Data.Tuple.Nested ((/\), type (/\))
import Data.Array ((:))
import Data.Array (snoc, dropEnd, length) as Array
import Data.Number (pi)
import Data.FunctorWithIndex (mapWithIndex)
import Data.String (take) as String

import Halogen as H
import Halogen.Aff as HA
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.HTML.Events as HE
import Halogen.Svg.Attributes as HSA
import Halogen.Svg.Attributes.Color as HSA
import Halogen.Svg.Attributes.FontSize (FontSize(..)) as HSA
import Halogen.Svg.Elements as HS

import Web.UIEvent.WheelEvent as Wheel

import Yoga.Tree (Tree)
import Yoga.Tree as Tree
import Yoga.Tree.Extended as Tree
import Yoga.Tree.Extended.Path (Path)
import Yoga.Tree.Extended.Path (advance, up, find, root, traverse, fill, toArray, Path(..)) as Path
import Yoga.Tree.Svg.Component.Tree.Graph as Render
import Yoga.Tree.Svg.Component.Tree.Render as Render
import Yoga.Tree.Svg.Component.Tree.Svg as Svg
import Yoga.Tree.Svg.Component.Tree.SvgAlt as SvgAlt
-- import Yoga.Tree.Zipper (Path)


type Slots =
  ( item :: forall q o. H.Slot q o Path
  )


_item  = Proxy :: _ "item"


data Action a
    = Receive (Input a)
    | WheelChange { dx :: Number, dy :: Number }
    | FocusOn Path
    | NodeClick Path a
    | NodeOver  Path a
    | NodeOut   Path a
    | ResetZoom
    | Advance Int
    | GoUp


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
    , preview :: Preview
    , zoom :: Number
    , size :: { width :: Number, height :: Number }
    }


component :: forall a query output m. Ord a => SvgAlt.Config a -> (forall cq co. H.Component cq (Path /\ a) co m) -> H.Component query (Input a) output m
component config childComp =
  H.mkComponent
    { initialState
    , render
    , eval: H.mkEval $ H.defaultEval
        { handleAction = handleAction
        , receive = Just <<< Receive
        }
    }
  where
  geometry :: SvgAlt.Geometry
  geometry =
    { scaleFactor : 10.0
    , baseRadius : 10.0
    , scaleLimit : { min : 0.2, max : 50.0 }
    }

  initialState :: Input a -> State a
  initialState { tree, size } =
    { tree, focus : Path.root, zoom : 1.0, size, preview : None }

  receive :: Input a -> State a -> State a
  receive { tree, size } =
    _ { size = size, tree = tree }

  events :: SvgAlt.Events (Action a) a
  events =
    { valueClick : NodeClick
    , valueOver  : NodeOver
    , valueOut   : NodeOut
    }

  rootButton :: (Path -> a -> String) -> Tree a -> _
  rootButton toLabel tree =
    let
      mbValueAt = tree # Path.find Path.root <#> Tree.value
      buttonLabel = maybe "*" (\val -> "* (" <> toLabel Path.root val <> ")") mbValueAt
    in  HH.button
          [ HP.style "cursor: pointer; pointer-events: all;"
          , HE.onClick $ const $ FocusOn $ Path.root
          ]
          [ HH.text buttonLabel ]

  pathButton :: (Path -> a -> String) -> Tree a -> Array Int -> Int -> Int -> _
  pathButton toLabel tree fullPath pIndex pValue =
    let
      pathLen = Array.length fullPath
      isLast = pIndex == (pathLen - 1)
      curPath = Path.Path $ Array.dropEnd (max 0 $ pathLen - pIndex - 1) fullPath
      mbValueAt = tree # Path.find curPath <#> Tree.value
      buttonLabel = maybe (show pValue) (\val -> show pValue <> " (" <> toLabel curPath val <> ")") mbValueAt
    in
    if not isLast
      then HH.button
          [ HP.style "cursor: pointer; pointer-events: all;"
          , HE.onClick $ const $ FocusOn curPath
          ]
          [ HH.text buttonLabel ]
      else HH.text buttonLabel

  previewAt tree previewPath =
    case Path.find previewPath tree of
      Just previewNode ->
        SvgAlt.renderPreview config previewPath $ Tree.value previewNode
      Nothing ->
        HH.text "?"

  render :: State a -> _
  render state =
    HH.div
      [ HP.style "oveflow: hidden;" ]
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
            $ SvgAlt.renderGraph (geometry { scaleFactor = state.zoom * 5.0 }) config events
            -- FIXME: passing `state.focus` is needed only because else we would first fill already focused `Tree` with `Paths` when converting it to `Graph`
            $ SvgAlt.toGraph' state.focus
            $ fromMaybe state.tree
            $ Path.find state.focus state.tree
      , HH.div
        [ HP.style "position: absolute; right: 0; top: 0;" ]
        [ HH.button
          [ HP.style "cursor: pointer; pointer-events: all;"
          , HE.onClick $ const ResetZoom
          ]
          [ HH.text "Reset Zoom" ]
        , HH.text $ "Zoom : " <> (String.take 6 $ show state.zoom)
        , HH.text $ "Size : " <> show state.size.width <> "x" <> show state.size.height
        ]
      , HH.div
          [ HP.style "position: absolute; right: 0; top: 20px;" ]
          $ pure
          $ case Path.toArray state.focus of
              [] -> HH.text "*"
              path ->
                HH.div
                  []
                  $ rootButton config.valueLabel state.tree
                  : mapWithIndex (pathButton config.valueLabel state.tree path) path
      , HH.div
          [ HP.style $ "position: absolute; right: 0; top: 100px; " <> case state.preview of
            Focused _ -> "opacity: 1.0;"
            LostFocus _ -> "opacity: 0.7;"
            None -> ""
          ]
          $ pure
          $ case state.preview of
              Focused previewPath ->
                previewAt state.tree previewPath
              LostFocus previewPath ->
                previewAt state.tree previewPath
              None ->
                HH.text ""
      ]

  {-
  render :: State a -> _
  render state =
    let
      aperture = 0.225
      renderPref =
        Render.defaults
          { ballRadius  = 10.0
          , halfAngle   = aperture * pi
          , scaleFactor = state.zoom
          }
      graph =
        Render.toGraph
          renderPref
          $ Path.fill
          $ state.tree
      config =
        { edgeColor  : const $ HSA.RGB 200 200 200
        , valueColor : const $ HSA.RGB   0 100   0
        , valueLabel : show
        }
    in
      HS.svg
        [ HSA.width  1000.0
        , HSA.height 1000.0
        , HE.onWheel \wevt -> WheelChange
            { dx : Wheel.deltaX wevt
            , dy : Wheel.deltaY wevt
            }
        ]
        $ Svg.render config Svg.FullLabel
        -- $ Svg.renderWithComponent childComp config
        $ Svg.transform 425.0 100.0 60.0 60.0 0.5
        $ graph
  -}

  {-
  render state =
    case Path.find state.focus $ Path.fill state.tree of
        Just tree ->
            renderNode tree
        Nothing -> HH.text "*"

  renderNode node =
    HH.div
        [ HP.style "padding-left: 10px;" ]
        $ (HH.slot_
            _item
            (Tuple.fst $ Tree.value node)
            childComp
            $ Tree.value node
          )
        : (foldl Array.snoc []
             $ renderNode
            <$> Tree.children node
          )
    -}

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
      H.modify_ _ { focus = path }
    NodeClick path val ->
      H.modify_ _ { focus = path }
    NodeOver path _ ->
      H.modify_ _ { preview = Focused path }
    NodeOut path _ ->
      H.modify_ _ { preview = LostFocus path }
    Advance n ->
      pure unit
    GoUp ->
      pure unit