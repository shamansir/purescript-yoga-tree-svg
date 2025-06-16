module Yoga.Tree.Svg.Component.Tree where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Foldable (foldl)
import Data.Traversable (traverse)

import Halogen as H
import Halogen.Aff as HA
import Halogen.HTML as HH
import Halogen.HTML.Events as HE

import Yoga.Tree (Tree)
import Yoga.Tree as Tree
import Yoga.Tree.Extended.Path (Path)
import Yoga.Tree.Extended.Path (advance, up, find, root, traverse) as Path
-- import Yoga.Tree.Zipper (Path)


foo = 42

data Action
    = Advance Int
    | GoUp


type State a =
    { tree :: Tree a
    , focus :: Path
    }


component ∷ ∀ query action output m. H.Component query action output m
component =
  H.mkComponent
    { initialState
    , render
    , eval: H.mkEval $ H.defaultEval { handleAction = handleAction }
    }
  where
  initialState _ =
    { tree : Tree.mkLeaf unit
    , focus : Path.root
    }

  render state =
    case Path.find state.focus state.tree of
        Just tree -> HH.text "some tree"
            -- foldl ?wh ?wh tree
        Nothing -> HH.text "*"
    -- HH.div_
    --   [ HH.button [ HE.onClick \_ -> Decrement ] [ HH.text "-" ]
    --   , HH.div_ [ HH.text $ show state ]
    --   , HH.button [ HE.onClick \_ -> Increment ] [ HH.text "+" ]
    --   ]

  handleAction = case _ of
    _ -> pure unit