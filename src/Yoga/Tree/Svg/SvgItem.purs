module Yoga.Tree.Svg.SvgItem where

import Data.Maybe (Maybe)


class IsSvgTreeItem a where
  default :: a
  toText :: a -> String
  fromText :: String -> Maybe a
