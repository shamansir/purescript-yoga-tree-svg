module Demo where

import Prelude

import Data.Maybe (Maybe(..), fromMaybe)
import Data.Int as Int

import Effect (Effect)
import Effect.Class (class MonadEffect)

import Halogen as H
import Halogen.Aff as HA
import Halogen.HTML as HH
-- import Halogen.HTML.Events as HE
import Halogen.Svg.Elements as HS
import Halogen.Svg.Attributes as HSA
import Halogen.VDom.Driver (runUI)
-- import Halogen.Subscription as HSS
import Halogen.Query.Event (eventListener)

import Type.Proxy (Proxy(..))

import Control.Comonad.Cofree ((:<))

import Yoga.Tree (Tree)
-- import Yoga.Tree as Tree
import Yoga.Tree.Extended ((:<~))
import Yoga.Tree.Extended as Tree
import Yoga.Tree.Svg (NodeComponent, component_) as YogaSvgTree
import Yoga.Tree.Svg.Render (Modes, Config) as YogaSvgTree
import Yoga.Tree.Svg.Render (NodeMode(..), EdgeMode(..)) as YST

import Web.Event.Event as E
import Web.HTML (window) as Web
import Web.HTML.Window (toEventTarget, innerWidth, innerHeight) as Window
-- import Web.UIEvent.KeyboardEvent (KeyboardEvent)
-- import Web.UIEvent.KeyboardEvent as KE
-- import Web.UIEvent.KeyboardEvent.EventTypes as KET


main :: Effect Unit
main = HA.runHalogenAff do
  body <- HA.awaitBody
  runUI component unit body


type Slots =
  ( tree :: forall q o. H.Slot q o Unit
  )


type State a =
  { tree :: Tree a
  , window :: Maybe { width :: Number, height :: Number }
  }


_tree  = Proxy :: _ "tree"


data Action
  = Initialize
  | HandleResize


type Item = Int


q :: forall n. n -> Tree n
q = Tree.leaf


ch :: forall n. Array n -> Array (Tree n)
ch = map Tree.leaf


{-

1
 11
 12
  121
   1211
   1212
  122
   1221
   1222
   1223
  123
  124
   1241
  125
 13
  131
  132
  133
  134
  135
  136
  137
 14
 15
 16
  161
-}


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


simpleTree :: Tree Item
simpleTree =
  1 :<~
    [ 11
    , 12
    , 13
    , 14
    , 15
    ]


config :: YogaSvgTree.Config Item
config =
  { edgeColor : \_ _ _ _ -> HSA.RGB 0 0 0
  , edgeLabel : \_ _ _ _ -> "E"
  , valueLabel : const show
  , valueColor : const $ const $ HSA.RGB 200 200 200
  , valueSize  : const $ const { width : 200.0, height : 25.0 }
  }


modes :: YogaSvgTree.Modes
modes =
  { nodeMode : YST.NodeWithLabel
  , previewMode : YST.Component -- YST.NodeWithLabel
  , edgeMode : YST.JustEdge -- EdgeWithLabel
  }


component ∷ ∀ query input output m. MonadEffect m => H.Component query input output m
component =
  H.mkComponent
    { initialState
    , render
    , eval: H.mkEval $ H.defaultEval
      { handleAction = handleAction
      , initialize = Just Initialize
      }
    }
  where
  initialState :: input -> State Item
  initialState _ = { tree : myTree, window : Nothing }

  defaultSize = { width : 1000.0, height : 1000.0 }

  reduceSize { width, height } = { width : width - 10.0, height : height - 10.0 }

  render :: forall action. State Item -> H.ComponentHTML action Slots m
  render state =
    HH.slot_ _tree unit
      (YogaSvgTree.component_ modes config child)
      { tree : state.tree
      , size : reduceSize $ fromMaybe defaultSize state.window
      }

  child :: YogaSvgTree.NodeComponent m Item
  child = H.mkComponent
    { initialState : identity
    , render : show >>> HH.text >>> pure >>> HS.text [ HSA.fill $ HSA.RGB 0 0 0 ]
    , eval: H.mkEval $ H.defaultEval { handleAction = const $ pure unit }
    }

  handleAction = case _ of
    Initialize -> do
      handleAction HandleResize

      window <- H.liftEffect $ Web.window

      H.subscribe' \_ ->
          eventListener
              (E.EventType "resize")
              (Window.toEventTarget window)
              (E.target >=> (const $ Just HandleResize))
    HandleResize -> do
      window    <- H.liftEffect $ Web.window
      newWidth  <- H.liftEffect $ Window.innerWidth window
      newHeight <- H.liftEffect $ Window.innerHeight window
      H.modify_ $ _ { window = Just { width : Int.toNumber newWidth, height : Int.toNumber newHeight } }
