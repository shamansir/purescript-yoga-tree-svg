module Yoga.Tree.TempConvert where

import Prelude

import Control.Comonad.Cofree (Cofree, head, mkCofree, tail, (:<))
import Control.Monad.Rec.Class (Step(..), tailRec)

import Data.Maybe (Maybe(..))
import Data.Array (snoc, uncons, replicate)
import Data.Tuple (fst, snd) as Tuple
import Data.Tuple.Nested ((/\), type (/\))
import Data.String (joinWith) as String

import Yoga.Tree (Tree, Forest)
import Yoga.Tree as Tree
import Yoga.Tree.Extended (value) as Tree
import Yoga.Tree.Extended.Path (Path)
import Yoga.Tree.Extended.Path (fill) as Path




type LinesWithPaths = Array (Path /\ String)
type RenderStep = { current ∷ Forest (Path /\ String), drawn ∷ LinesWithPaths, level ∷ Int }


toLines :: (Int -> Path -> String) -> Tree String -> Array String
toLines prefixGen = toLines' prefixGen >>> map Tuple.snd


toLines' :: (Int -> Path -> String) -> Tree String -> LinesWithPaths
toLines' prefixGen tree =
    tailRec go { level: 0, drawn: [ head t ], current: (tail t) }
    where
    t :: Tree (Path /\ String)
    t = Path.fill tree
    go :: RenderStep -> Step RenderStep LinesWithPaths
    go x = case x { current = uncons x.current } of
        { drawn: s, current: Nothing } -> Done s
        { level: l, drawn: s, current: Just { head: c, tail: cs } } ->
            let
                curPath = Tuple.fst $ head c :: Path
                curVal  = Tuple.snd $ head c :: String
                drawn = prefixGen l curPath <> curVal -- (power "       " l) <> "|----> " <> (head c) <> "\n"
            in
                Loop { level: l, drawn: s <> pure (curPath /\ drawn) <> (tailRec go { level: l + 1, drawn: [], current: (tail c) }), current: cs }


toString :: forall a. (a -> String) -> Tree a -> String
toString convert = map convert >>> toLines (\n _ -> String.joinWith "" $ replicate n " ") >>> String.joinWith "\n"