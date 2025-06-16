module Yoga.Tree.Svg.Component.Tree where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Foldable (foldl, foldr)
import Data.Traversable (traverse)
import Data.Tuple (uncurry)
import Data.Tuple.Nested ((/\), type (/\))
import Data.Array ((:))
import Data.Array (snoc) as Array

import Halogen as H
import Halogen.Aff as HA
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.HTML.Events as HE

import Yoga.Tree (Tree)
import Yoga.Tree as Tree
import Yoga.Tree.Extended as Tree
import Yoga.Tree.Extended.Path (Path)
import Yoga.Tree.Extended.Path (advance, up, find, root, traverse, fill) as Path
-- import Yoga.Tree.Zipper (Path)


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


component :: forall a query output m. Show a => H.Component query (Input a) output m
component =
  H.mkComponent
    { initialState
    , render
    , eval: H.mkEval $ H.defaultEval { handleAction = handleAction }
    }
  where
  initialState { tree } =
    { tree, focus : Path.root }

  render state =
    case Path.find state.focus $ Path.fill state.tree of
        Just tree ->
            renderNode tree
        Nothing -> HH.text "*"

  renderNode node =
    HH.div
        [ HP.style "padding-left: 10px;" ]
        $ (HH.text $ show $ Tree.value node)
        : (foldl Array.snoc []
             $ renderNode
            <$> Tree.children node
          )

    -- HH.div_
    --   [ HH.button [ HE.onClick \_ -> Decrement ] [ HH.text "-" ]
    --   , HH.div_ [ HH.text $ show state ]
    --   , HH.button [ HE.onClick \_ -> Increment ] [ HH.text "+" ]
    --   ]

  handleAction = case _ of
    _ -> pure unit