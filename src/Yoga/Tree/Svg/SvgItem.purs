module Yoga.Tree.Svg.SvgItem where

import Data.Maybe (Maybe)

import Yoga.JSON (class WriteForeign, class ReadForeign)


class (WriteForeign a, ReadForeign a) <= IsSvgTreeItem a where
  default :: a
  toText :: a -> String
  fromText :: String -> Maybe a
