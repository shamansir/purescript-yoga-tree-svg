module Yoga.Tree.Svg.Component.Tree where

import Prelude

import Type.Proxy (Proxy(..))

import Data.Maybe (Maybe(..))
import Data.Foldable (foldl, foldr)
import Data.Traversable (traverse)
import Data.Tuple (uncurry)
import Data.Tuple (fst, snd) as Tuple
import Data.Tuple.Nested ((/\), type (/\))
import Data.Array ((:))
import Data.Array (snoc) as Array
import Data.Number (pi)

import Halogen as H
import Halogen.Aff as HA
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.HTML.Events as HE
import Halogen.Svg.Attributes as HSA
import Halogen.Svg.Attributes.Color as HSA
import Halogen.Svg.Attributes.FontSize (FontSize(..)) as HSA
import Halogen.Svg.Elements as HS


import Yoga.Tree (Tree)
import Yoga.Tree as Tree
import Yoga.Tree.Extended as Tree
import Yoga.Tree.Extended.Path (Path)
import Yoga.Tree.Extended.Path (advance, up, find, root, traverse, fill) as Path
import Yoga.Tree.Svg.Component.Tree.Graph as Render
import Yoga.Tree.Svg.Component.Tree.Render as Render
import Yoga.Tree.Svg.Component.Tree.Svg as Svg
-- import Yoga.Tree.Zipper (Path)


type Slots =
  ( item :: forall q o. H.Slot q o Path
  )


_item  = Proxy :: _ "item"


data Action
    = Advance Int
    | GoUp


type Input a =
    { tree :: Tree a
    }


type State a =
    { tree :: Tree a
    , focus :: Path
    }


component :: forall a query output m. Show a => Ord a => (forall cq co. H.Component cq (Path /\ a) co m) -> H.Component query (Input a) output m
component childComp =
  H.mkComponent
    { initialState
    , render
    , eval: H.mkEval $ H.defaultEval { handleAction = handleAction }
    }
  where
  initialState { tree } =
    { tree, focus : Path.root }

  render state =
    let
      aperture = 0.225
      renderPref =
        Render.defaults
          { ballRadius  = 10.0
          , halfAngle   = aperture * pi
          , scaleFactor = 0.8
          }
      graph =
        Render.toGraph
          renderPref
          state.tree
      config =
        { edgeColor  : const $ HSA.RGB 200 200 200
        , valueColor : const $ HSA.RGB   0 100   0
        , valueLabel : show
        }
    in
      HS.svg
        [ HSA.width 1000.0
        , HSA.height 1000.0
        ]
        $ Svg.render config Svg.FullLabel
        $ Svg.transform 425.0 100.0 60.0 60.0 0.5
        $ graph

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

    -- HH.div_
    --   [ HH.button [ HE.onClick \_ -> Decrement ] [ HH.text "-" ]
    --   , HH.div_ [ HH.text $ show state ]
    --   , HH.button [ HE.onClick \_ -> Increment ] [ HH.text "+" ]
    --   ]

  handleAction = case _ of
    _ -> pure unit