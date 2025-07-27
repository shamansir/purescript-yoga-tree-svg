module Yoga.Tree.Svg.ExportMode where

import Prelude

import Data.Maybe (Maybe(..))

import Data.Set (Set)
import Data.Set (fromFoldable) as Set

import Yoga.Tree (Tree)
import Yoga.Tree.Extended.Convert as TreeConv


newtype ExportId
    = ExportId String


derive instance Eq ExportId
derive instance Ord ExportId
instance Show ExportId where show (ExportId eid) = eid


data ExportMode
    = Json
    | Text TreeConv.Mode
    | Custom ExportId


nonCustom :: Set ExportMode
nonCustom = Set.fromFoldable
    [ Json, Text TreeConv.Lines, Text TreeConv.Indent, Text TreeConv.Paths, Text TreeConv.Corners, Text TreeConv.Triangles, Text TreeConv.Dashes ]


label :: ExportMode -> String
label = case _ of
  Json -> "JSON"
  Custom (ExportId id) -> id
  Text TreeConv.Lines -> "Lines"
  Text TreeConv.Indent -> "Indent"
  Text TreeConv.Paths -> "Paths"
  Text TreeConv.Corners -> "Corners"
  Text TreeConv.Triangles -> "Triangles"
  Text TreeConv.Dashes -> "Dashes"


derive instance Eq ExportMode
derive instance Ord ExportMode


class CustomTreeExport a where
    exportTree :: ExportId -> Tree a -> Maybe String


noCustomExport :: forall a. ExportId -> Tree a -> Maybe String
noCustomExport = const $ const $ Nothing