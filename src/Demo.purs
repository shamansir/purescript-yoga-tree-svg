module Demo where

import Prelude

import Data.Tuple.Nested ((/\), type (/\))

import Effect (Effect)

import Halogen as H
import Halogen.Aff as HA
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.VDom.Driver (runUI)

import Type.Proxy (Proxy(..))

import Control.Comonad.Cofree ((:<))

import Yoga.Tree (Tree)
-- import Yoga.Tree as Tree
import Yoga.Tree.Extended as Tree
import Yoga.Tree.Extended.Path (Path)
import Yoga.Tree.Svg.Component.Tree as YogaSvgTree

main :: Effect Unit
main = HA.runHalogenAff do
  body <- HA.awaitBody
  runUI component unit body


type Slots =
  ( tree :: forall q o. H.Slot q o Unit
  )


type State a =
  { tree :: Tree a
  }


_tree  = Proxy :: _ "tree"


data Action
  = Initialize


type Item = Int


q = Tree.leaf


ch = map Tree.leaf


myTree :: Tree Item
myTree =
  1 :<
    [ q 11
    , 12 :<
      [ 121 :< ch [ 1211, 1212 ]
      , 122 :< ch [ 1221, 1222, 1223 ]
      , q 123
      , 124 :< ch [ 1241 ]
      , q 125
      ]
    , 13 :< ch [ 131, 132, 133, 134, 135, 136, 137 ]
    , q 14
    , q 15
    , 16 :< ch [ 161 ]
    ]


component ∷ ∀ query input output m. H.Component query input output m
component =
  H.mkComponent
    { initialState
    , render
    , eval: H.mkEval $ H.defaultEval { handleAction = handleAction }
    }
  where
  initialState :: input -> State Item
  initialState _ = { tree : myTree }

  render :: forall action. State Item -> H.ComponentHTML action Slots m
  render state =
    HH.slot_ _tree unit (YogaSvgTree.component child) { tree : state.tree }

  child :: forall q o. H.Component q (Path /\ Item) o m
  child = H.mkComponent
    { initialState : identity
    , render : show >>> HH.text
    , eval: H.mkEval $ H.defaultEval { handleAction = const $ pure unit }
    }

  handleAction = const $ pure unit