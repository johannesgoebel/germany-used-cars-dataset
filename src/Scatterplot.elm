module Scatterplot exposing (..)
import Axis
import Html exposing (Html)
import Scale exposing (ContinuousScale)
import Statistics
import TypedSvg exposing (circle, g, rect, style, svg, text_)
import TypedSvg.Attributes exposing (class, fontFamily, fontSize, textAnchor, transform, viewBox)
import TypedSvg.Attributes.InPx exposing (cx, cy, height, r, width, x, y)
import TypedSvg.Core exposing (Svg, text)
import TypedSvg.Types exposing (AnchorAlignment(..), Length(..), Transform(..))


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


defaultExtent : ( number, number1 )
defaultExtent =
     ( 0, 100 ) 

drawScatterplot: List String -> List Float -> List Float -> String -> String -> Svg msg 
drawScatterplot pointLabels xValues yValues xLabel yLabel =
    let
        points : List (Float, Float)
        points =
            List.map2 (\x y-> (x,y)) xValues yValues
            
                
        xScale : List Float -> ContinuousScale Float
        xScale values =
                Scale.linear ( 0, w - 2 * padding ) ( wideExtent values )


        yScale : List Float -> ContinuousScale Float
        yScale values =
                 Scale.linear ( h - 2 * padding, 0 ) ( wideExtent values )
                 
        
        xScaleLocal : ContinuousScale Float
        xScaleLocal =
            xScale xValues

        yScaleLocal : ContinuousScale Float
        yScaleLocal =
            yScale yValues

        half : ( Float, Float ) -> Float
        half t =
            (Tuple.second t - Tuple.first t) / 2

        labelPositions : { x : Float, y : Float }
        labelPositions =
            { x = wideExtent xValues |> half
            , y = wideExtent yValues |> Tuple.second
            }
              
        xAxis : List Float -> Svg msg
        xAxis values =
            Axis.bottom [ Axis.tickCount tickCount ] (xScale values)


        yAxis : List Float -> Svg msg
        yAxis values =
            Axis.left [ Axis.tickCount tickCount ] (yScale values)   
            
        wideExtent : List Float -> ( Float, Float )
        wideExtent values =
            Statistics.extent ( values) |> Maybe.withDefault (0,0)|> Tuple.mapFirst (\x -> x - (Maybe.withDefault 0 (List.maximum values)) / 20) |> Tuple.mapSecond (\x -> x +  Maybe.withDefault 0 (List.maximum values) / 10)
          

        makePoint : ContinuousScale Float -> ContinuousScale Float -> (Float, Float) -> String -> Svg msg
        makePoint scaleX scaleY point pointLabelText=
            g [ class [ "point" ], fontSize <| Px 10.0, fontFamily [ "sans-serif" ] ]       
                [ circle
                    [ cx (Scale.convert scaleX (Tuple.first point))
                    , cy (Scale.convert scaleY (Tuple.second point))
                    , r radius               
                    ]
                    [] 
                    , text_
                    [ y (Scale.convert scaleY (Tuple.first point))
                    , x (Scale.convert scaleX (Tuple.second point))
                    ]
                    [ text (pointLabelText) ]
                ]
              
    in            
    
    svg [ viewBox 0 0 w h, TypedSvg.Attributes.width <| TypedSvg.Types.Percent 100, TypedSvg.Attributes.height <| TypedSvg.Types.Percent 100 ]
        [ style [] [ TypedSvg.Core.text """
            .point circle { stroke: rgba(0, 0, 0,0.4); fill: rgba(255, 255, 255,0.3); }
            .point text { display: none; }
            .point:hover circle { stroke: rgba(0, 0, 0,1.0); fill: rgb(118, 214, 78); }
            .point:hover text { display: inline; }
          """ ]
        , g
            [ transform [ Translate (padding - 1) ( padding - 1 ) ]
            , fontSize <| Px 10.0
            , fontFamily [ "sans-serif" ]
            , class [ "r", "point" ]
            ]
            []
        , g 
            [ transform [ Translate (padding - 1) (h - padding) ] ]
            [ xAxis xValues
             , text_
                [ x <| Scale.convert xScaleLocal labelPositions.x
                 , y 35                
                ]
                [ text xLabel ]
            ]
        , g 
            [transform [ Translate (padding - 1) padding ] ]
            [ yAxis yValues                             
            , text_
                [ x -45
                , y <| Scale.convert yScaleLocal labelPositions.y     
                ]
                [ text yLabel ]         
            ]
         , g 
             [transform [ Translate padding padding ] ]
                (List.map2 (makePoint xScaleLocal yScaleLocal)  points pointLabels)
        ]