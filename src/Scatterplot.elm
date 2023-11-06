module Scatterplot exposing (..)
import TypedSvg exposing (g, style, svg, text_)
import TypedSvg.Attributes.InPx exposing ( x, y)
import TypedSvg.Attributes exposing (class, fontFamily, fontSize, transform, viewBox)
import TypedSvg.Types exposing (percent)
import TypedSvg.Core exposing (Svg)
import Scale exposing (ContinuousScale)
import Axis
import Statistics

w : Float
w =
    900


h : Float
h =
    450


padding : Float
padding =
    60


radius : Float
radius =
    5.0


tickCount : Int
tickCount =
    5


xAxis : List Float -> Svg msg
xAxis values =
    Axis.bottom [ Axis.tickCount tickCount ] (xScale values)


yAxis : List Float -> Svg msg
yAxis values =
    Axis.left [ Axis.tickCount tickCount ] (yScale values)


xScale : List Float -> ContinuousScale Float
xScale values =
    Scale.linear ( 0, w - 2 * padding ) (wideExtent values)


yScale : List Float -> ContinuousScale Float
yScale values =
    Scale.linear ( h - 2 * padding, 0 ) (wideExtent values)


wideExtent : List Float -> ( Float, Float )
wideExtent values =
    let
        closeExtent =
            Statistics.extent values
                |> Maybe.withDefault defaultExtent

        extension =
            (Tuple.second closeExtent - Tuple.first closeExtent) / toFloat (2 * tickCount)
    in
    ( Tuple.first closeExtent - extension |> max 0
    , Tuple.second closeExtent + extension
    )


defaultExtent : ( number, number1 )
defaultExtent =
    ( 0, 100 )


-- drawScatterplot: List String -> List Float -> List Float -> String -> Svg msg 
-- drawScatterplot pointLabel xValues yValues xLabel yLabel =


