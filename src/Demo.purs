module Demo where

import Prelude

import Debug as Debug

import Foreign (F)

import Data.Maybe (Maybe(..), fromMaybe)
import Data.Int as Int
import Data.Array (replicate) as Array
import Data.FunctorWithIndex (mapWithIndex)
import Data.String (length) as String

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
import Yoga.Tree.Svg (NodeComponent, component_, allElements) as YST
import Yoga.Tree.Svg.Render (Modes, Config, NodeMode(..), EdgeMode(..)) as YST
import Yoga.Tree.Svg.Style (Mode(..)) as YST
import Yoga.Tree.Svg.SvgItem (class IsSvgTreeItem)
import Yoga.Tree.Svg.SvgItem (toText) as YSTI
import Yoga.JSON (class WriteForeign, class ReadForeign, readImpl, writeImpl)

import Web.Event.Event as E
import Web.HTML (window) as Web
import Web.HTML.Window (toEventTarget, innerWidth, innerHeight) as Window
-- import Web.UIEvent.KeyboardEvent (KeyboardEvent)
-- import Web.UIEvent.KeyboardEvent as KE
-- import Web.UIEvent.KeyboardEvent.EventTypes as KET


demoTree :: Tree IntItem
demoTree = myTree


main :: Effect Unit
main = HA.runHalogenAff do
  body <- HA.awaitBody
  runUI (component demoTree) unit body


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


newtype IntItem = IntItem Int


ii :: Int -> IntItem
ii = IntItem


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


myTree :: Tree IntItem
myTree =
  ii 1 :<
    [ q $ ii 11
    , ii 12 :<
      [ ii 121 :< ch [ ii 1211, ii 1212 ]
      , ii 122 :< ch [ ii 1221, ii 1222, ii 1223 ]
      , q $ ii 123
      , ii 124 :< ch [ ii 1241 ]
      , q $ ii 125
      ]
    , ii 13 :< (ch $ ii <$> [ 131, 132, 133, 134, 135, 136, 137 ])
    , q $ ii 14
    , q $ ii 15
    , ii 16 :< ch [ ii 161 ]
    ]


simpleTree :: Tree IntItem
simpleTree =
  ii 1 :<~
    (ii <$>
      [ 11
      , 12
      , 13
      , 14
      , 15
      ]
    )


manyItems :: Tree IntItem
manyItems =
  ii 1 :<
    [ ii 500 :<~ (ii <$> (mapWithIndex const $ Array.replicate 500 unit))
    , ii 200 :<~ (ii <$> (mapWithIndex const $ Array.replicate 200 unit))
    , ii 25  :<~ (ii <$> (mapWithIndex const $ Array.replicate  25 unit))
    , ii 30  :<~ (ii <$> (mapWithIndex const $ Array.replicate  30 unit))
    , ii 100 :<~ (ii <$> (mapWithIndex const $ Array.replicate 100 unit))
    ]


derive newtype instance Show IntItem
instance IsSvgTreeItem IntItem where
  toText = show
  fromText = Int.fromString >>> map IntItem
  default = IntItem $ -1


instance WriteForeign IntItem where
  writeImpl (IntItem int) = writeImpl int
instance ReadForeign IntItem where
  readImpl f = (readImpl f :: F Int) <#> IntItem


config :: forall a. IsSvgTreeItem a => YST.Config a
config =
  { edgeColor : \_ _ _ _ -> HSA.RGB 0 0 0
  , edgeLabel : \_ _ _ _ -> "E"
  , valueLabel : const YSTI.toText
  , valueLabelColor : const $ const $ HSA.RGB 10 10 10
  , valueLabelWidth : \path -> String.length <<< config.valueLabel path
  , valueColor : const $ const $ HSA.RGB 200 200 200
  , componentSize  : const $ const { width : 200.0, height : 25.0 }
  }


modes :: YST.Modes
modes =
  { nodeMode : YST.NodeWithLabel
  , previewMode : YST.Component -- YST.NodeWithLabel
  , edgeMode : YST.JustEdge -- EdgeWithLabel
  }


component ∷ ∀ a query input output m. MonadEffect m => IsSvgTreeItem a => Tree a -> H.Component query input output m
component startFromTree =
  H.mkComponent
    { initialState
    , render
    , eval: H.mkEval $ H.defaultEval
      { handleAction = handleAction
      , initialize = Just Initialize
      }
    }
  where
  initialState :: input -> State a
  initialState _ = { tree : startFromTree, window : Nothing }

  defaultSize = { width : 1000.0, height : 1000.0 }

  reduceSize { width, height } = { width : width - 10.0, height : height - 10.0 }

  render :: forall action. State a -> H.ComponentHTML action Slots m
  render state =
    HH.slot_ _tree unit
      (YST.component_ modes config child)
      { tree : state.tree
      , size : reduceSize $ fromMaybe defaultSize state.window
      , elements : YST.allElements
      , mode : YST.Light
      }

  child :: YST.NodeComponent m a
  child = H.mkComponent
    { initialState : identity
    , render : childString >>> HH.text >>> pure >>> HS.text [ HSA.fill $ HSA.RGB 0 0 0 ]
    , eval: H.mkEval $ H.defaultEval { handleAction = const $ pure unit }
    }

  childString { path, value } =
    show path <> " // " <> YSTI.toText value

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
