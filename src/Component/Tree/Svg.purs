module Yoga.Tree.Svg.Component.Tree.Svg where

import Prelude

import Data.Array (take, concat, head) as Array
import Data.String (split, Pattern(..)) as String
import Data.Maybe (fromMaybe)

import Halogen as H
import Halogen.HTML (HTML)
import Halogen.HTML as HH
import Halogen.HTML.Properties as HHP
import Halogen.HTML.Events as HE
import Halogen.Svg.Attributes as HSA
import Halogen.Svg.Attributes.FontSize (FontSize(..)) as HSA
import Halogen.Svg.Elements as HS

import Yoga.Tree.Svg.Component.Tree.Graph (Graph, Edge, Node)

-- translated from: https://github.com/jxxcarlson/elm-tree-builder/blob/7.0.0/src/Tree/Svg.elm


{-| -}
data LabelStyle
    = NoLabel
    | FullLabel
    | FirstWord


type Config e a =
    { edgeColor  :: e -> HSA.Color
    , valueColor :: a -> HSA.Color
    , valueLabel :: a -> String
    }


{-| Transform: shift by dx, dy and rescale by sx,sy,sr where the
arguments are dx, dy, sx, sy, sr, graph
|-}
transform :: forall e a. Number -> Number -> Number -> Number -> Number -> Graph e a -> Graph e a
transform dx dy sx sy sr graph =
    transformEdge dx dy sx sy sr <$> graph


{-| Render a graph to SVG
|-}
render :: forall e a p i. Config e a -> LabelStyle -> Graph e a -> Array (HTML p i)
render config labelStyle graph =
    (renderFirstEdge config labelStyle <$> Array.take 1 graph) <> (renderEdge config labelStyle <$> graph) # Array.concat


{-| Translate and rescale and edge.
|-}
transformEdge :: forall e a. Number -> Number -> Number -> Number -> Number -> Edge e a -> Edge e a
transformEdge dx dy sx sy sr edge =
    edge { from = transformNode dx dy sx sy sr edge.from, to = transformNode dx dy sx sy sr edge.to }



transformNode :: forall a. Number -> Number -> Number -> Number -> Number -> Node a -> Node a
transformNode dx dy sx sy sr node =
    node { x = sx * node.x + dx, y = sy * node.y + dy, r = sr * node.r }


renderFirstEdge :: forall e a p i. Config e a -> LabelStyle -> Edge e a -> Array (HTML p i)
renderFirstEdge config labelStyle edge =
    [ svgLine edge.from.x edge.from.y edge.to.x edge.to.y $ config.edgeColor edge.edge
    , renderNodeWithLabel config.valueLabel config.valueColor labelStyle edge.from
    , renderNode config.valueColor edge.to
    ]


renderEdge :: forall e a p i. Config e a -> LabelStyle -> Edge e a -> Array (HTML p i)
renderEdge config labelStyle edge =
    [ svgLine edge.from.x edge.from.y edge.to.x edge.to.y $ config.edgeColor edge.edge
    , renderNode config.valueColor edge.from
    , renderNodeWithLabel config.valueLabel config.valueColor labelStyle edge.to
    ]


renderNodeWithLabel :: forall a p i. (a -> String) -> (a -> HSA.Color) -> LabelStyle -> Node a -> HTML p i
renderNodeWithLabel valueToLabel valueToColor labelStyle node =
    let
        label =
            case labelStyle of
                NoLabel ->
                    ""

                FullLabel ->
                    valueToLabel node.val

                FirstWord ->
                    valueToLabel node.val # String.split (String.Pattern " ") # Array.head # fromMaybe ""
    in
    HS.g []
        [ svgCircle node.x node.y node.r $ valueToColor node.val
        , HS.text
            [ HSA.x $ node.x + 12.0
            , HSA.y $ node.y + 0.0
            , HSA.fill $ valueToColor node.val
            ]
            [ HH.text label ]
        ]



renderNode :: forall a p i. (a -> HSA.Color) -> Node a -> HTML p i
renderNode valueToColor node =
    HS.g [ ]
        [ svgCircle node.x node.y node.r $ valueToColor node.val
        ]



svgCircle :: forall p i. Number -> Number -> Number -> HSA.Color -> HTML p i
svgCircle x y rr color =
    HS.circle
        [ HSA.cx x
        , HSA.cy y
        , HSA.r rr
        , HSA.fill color
        ]



svgLine :: forall p i. Number -> Number -> Number -> Number -> HSA.Color -> HTML p i
svgLine x1_ y1_ x2_ y2_ color =
    HS.line
        [ HSA.x1 x1_
        , HSA.y1 y1_
        , HSA.x2 x2_
        , HSA.y2 y2_
        , HSA.stroke color
        ]