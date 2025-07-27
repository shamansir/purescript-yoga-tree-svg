module Yoga.Tree.Svg.TreeValue where

import Data.Maybe (Maybe)

import Halogen.Svg.Attributes (Color) as HSA

import Yoga.JSON (class WriteForeign, class ReadForeign)
import Yoga.Tree.Extended.Path (Path)

import Yoga.Tree.Svg.Style (Theme)
import Yoga.Tree.Svg.ExportMode (ExportMode)

class (WriteForeign a, ReadForeign a) <= IsTreeValue a where
  default :: a
  _export :: ExportMode -> a -> String
  _import :: ExportMode -> String -> Maybe a


class (IsTreeValue a) <= IsSvgTreeValue a where
  valueLabel :: Path -> a -> String
  valueColor :: Theme -> Path -> a -> HSA.Color
  valueLabelColor :: Theme -> Path -> a -> HSA.Color
  valueLabelWidth :: Path -> a -> Int
  edgeColor :: Theme -> Path -> a -> Path -> a -> HSA.Color
  edgeLabel :: Path -> a -> Path -> a -> String
  componentSize :: Path -> a -> { width :: Number, height :: Number }

  foldLabel :: a -> String
  pathLabel :: Path -> a -> String
  pinnedLine :: Path -> a -> String