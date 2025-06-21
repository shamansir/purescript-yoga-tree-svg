module Yoga.Tree.Svg.Geometry
    ( findPosition
    , rotateBy
    , Position, Size
    , Positioned, PositionedMap, PositionedGraphMap
    , transform, scale
    ) where

import Prelude

import Data.Map (Map)
import Data.Int (toNumber) as Int
import Data.Number (cos, sin) as Number
import Data.Tuple.Nested (type (/\))
import Data.List (List)

import Yoga.Tree.Extended.Path (Path)


findPosition :: Number -> Position -> Number -> Number -> Int -> Int -> Position
findPosition baseRadius parent startAngle endAngle count index =
    let
        currentAngle = startAngle + ((endAngle - startAngle) / Int.toNumber count) * Int.toNumber index

        co = Number.cos currentAngle
        si = Number.sin currentAngle

        x = parent.x + (co * baseRadius - si * baseRadius)
        y = parent.y + (si * baseRadius + co * baseRadius)
    in
    { x, y }


rotateBy :: Position -> Position -> Number -> Number -> Position
rotateBy parent current scaleFactor theta =
    let
        dx = current.x - parent.x
        dy = current.y - parent.y
        co = Number.cos theta
        si = Number.sin theta
        xx = parent.x + scaleFactor * (co * dx - si * dy)
        yy = parent.y + scaleFactor * (si * dx + co * dy)
    in
    { x : xx, y : yy }


type Position = { x :: Number, y :: Number }
type Size = { width :: Number, height :: Number }
type Positioned a = { x :: Number, y :: Number, width :: Number, height :: Number, value :: a }
type PositionedMap a = Map Path (Positioned a)
type PositionedGraphMap a = Map Path (Positioned a /\ List Path)


transform :: forall a. Number -> Number -> Number -> Number -> Positioned a -> Positioned a
transform dx dy sx sy node =
    node { x = sx * node.x + dx, y = sy * node.y + dy, width = sx * node.width, height = sy * node.height }


scale :: forall a. Number -> Positioned a -> Positioned a
scale factor =
    transform 0.0 0.0 factor factor
