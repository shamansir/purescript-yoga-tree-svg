module Demo where

import Prelude

import Effect (Effect)
import Halogen as H
import Halogen.Aff as HA
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.VDom.Driver (runUI)

import Type.Proxy (Proxy(..))

import Yoga.Tree (Tree)
import Yoga.Tree as Tree
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


myTree :: Tree Item
myTree =
  Tree.mkTree 1
    [ Tree.mkLeaf 11
    , Tree.mkTree 12
      [ Tree.mkTree 121 [ Tree.mkLeaf 1211, Tree.mkLeaf 1212 ]
      , Tree.mkTree 122 [ Tree.mkLeaf 1221, Tree.mkLeaf 1222, Tree.mkLeaf 1223 ]
      , Tree.mkLeaf 123
      , Tree.mkTree 124 [ Tree.mkLeaf 1241 ]
      , Tree.mkLeaf 125
      ]
    , Tree.mkTree 13 [ Tree.mkLeaf 131, Tree.mkLeaf 132, Tree.mkLeaf 133, Tree.mkLeaf 134, Tree.mkLeaf 135, Tree.mkLeaf 136, Tree.mkLeaf 137 ]
    , Tree.mkLeaf 14
    , Tree.mkLeaf 15
    , Tree.mkTree 16
      [ Tree.mkLeaf 161 ]
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
    HH.slot_ _tree unit YogaSvgTree.component { tree : state.tree }

  handleAction = const $ pure unit