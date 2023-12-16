module StarPlot exposing (..)
import CarOfferTypes exposing (CarOfferData)
import CarOfferTypes exposing (Msg)
import TypedSvg.Core exposing (Svg)
import TypedSvg exposing ( ..)
import Html
import Scale exposing (convert, ContinuousScale)
import Color
import TypedSvg.Attributes exposing (..)

-- import Svg exposing (Svg, svg, circle, line, Attributes)

xScale : List Float -> ContinuousScale Float
xScale values =
        Scale.linear ( 0, w - 2 * padding ) ( wideExtent  )


yScale : List Float -> ContinuousScale Float
yScale values =
            Scale.linear ( h - 2 * padding, 0 ) ( wideExtent  )

padding = 10

h = 12

w = 12

wideExtent = (90, 90)

drawStarplot: List CarOfferData -> String -> Svg msg
drawStarplot carList param =
    let
        radius = 50.0
    in
        circle
                [ cx (Scale.convert xScale 0)
                , cy (Scale.convert yScale 0)
                , r (Scale.convert yScale radius)               
                ]
-- spokes : Float -> Svg msg
-- spokes radius =
--     let
--         angles =
--             List.map (\n -> (2 * Float.pi / 5) * Float.fromInt n) [ 0 .. 4 ]
--     in
--     List.map (\angle -> line [ Attributes.x1 "50", Attributes.y1 "50", Attributes.x2 <| toString <| 50 + radius * cos angle, Attributes.y2 <| toString <| 50 + radius * sin angle ] []) angles