module StarPlot exposing (..)
import CarOfferTypes exposing (CarOfferData)
import CarOfferTypes exposing (Msg)
import TypedSvg.Core exposing (Svg)
import TypedSvg exposing ( ..)
import Html
import Color
import TypedSvg.Attributes exposing (..)
-- import Svg exposing (Svg, svg, circle, line, Attributes)
drawStarplot: List CarOfferData -> String -> Svg msg
drawStarplot carList param =
    let
        radius = 50.0
    in
        svg [ width (px 120), height (px 120), viewBox 0 0 120 120 ]
            [ circle [ cx "50", cy "50", r (String.fromFloat radius) ] []
            -- , spokes radius
            ]

-- spokes : Float -> Svg msg
-- spokes radius =
--     let
--         angles =
--             List.map (\n -> (2 * Float.pi / 5) * Float.fromInt n) [ 0 .. 4 ]
--     in
--     List.map (\angle -> line [ Attributes.x1 "50", Attributes.y1 "50", Attributes.x2 <| toString <| 50 + radius * cos angle, Attributes.y2 <| toString <| 50 + radius * sin angle ] []) angles
