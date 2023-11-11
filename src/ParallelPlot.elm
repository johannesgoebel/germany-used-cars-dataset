module ParallelPlot exposing (..)
import CarOfferTypes exposing (ParallelAxisCarOffer)
import Array
import Axis
import Browser
import Color exposing (Color)
import Html exposing (Html)
import Html.Events exposing (onClick)
import List.Extra
import Path
import Scale exposing (ContinuousScale)
import Shape
import Statistics
import TypedSvg exposing (circle, g, rect, style, svg, text_)
import TypedSvg.Attributes exposing (class, fill, fontFamily, fontSize, stroke, strokeWidth, textAnchor, transform, viewBox)
import TypedSvg.Attributes.InPx exposing (cx, cy, height, r, width, x, y)
import TypedSvg.Core exposing (Svg, text)
import TypedSvg.Types exposing (AnchorAlignment(..), Length(..), Paint(..), Transform(..))


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


numberaxis : Float
numberaxis =
    4


wideExtent : List Float -> ( Float, Float )
wideExtent value =
    let
        nearextent =
            Statistics.extent value
                |> Maybe.withDefault defaultExtent

        extent =
            (Tuple.second nearextent - Tuple.first nearextent) / (2 * numberaxis)
    in
    ( Tuple.first nearextent - extent |> max 0
    , Tuple.second nearextent + extent
    )

drawParallelplot:  List ParallelAxisCarOffer -> String -> String -> String -> String -> Svg msg
drawParallelplot axisList str1 str2 str3 str4= 
       let
           listAxisLists: List (List ParallelAxisCarOffer)
           listAxisLists 
                  =  [axisList, axisList, axisList, axisList]
                  
           transformList : List (List Float)
           transformList =
              axisList
                |> List.map .values
                |> List.Extra.transpose

           axisLabelText: List String
           axisLabelText = 
                [str1, str2, str3, str4]
           listWidth :  List ( Float, Float ) 
           listWidth =
               transformList |> List.map wideExtent
               
           listeSkala =
            List.map (Scale.linear ( h, 0 )) listWidth
            
           listeAchse =
            List.map (Axis.left [ Axis.tickCount (round numberaxis) ]) listeSkala

           xSkala =
            Scale.linear ( 0, w ) ( 1, 4 ) 
            

       in
        svg
        [ viewBox 0 0 (w + 2 * padding) (h + 2 * padding)
        , TypedSvg.Attributes.width <| TypedSvg.Types.Percent 90
        , TypedSvg.Attributes.height <| TypedSvg.Types.Percent 90
        , TypedSvg.Attributes.preserveAspectRatio (TypedSvg.Types.Align TypedSvg.Types.ScaleMin TypedSvg.Types.ScaleMin) TypedSvg.Types.Slice
        ]
        <|
        [ TypedSvg.style []
            [
                TypedSvg.Core.text """
                .parallelerPunkt { stroke: rgba(1, 0, 0,0.2);}
                .parallelerPunkt:hover {stroke: rgb(173, 255, 47); stroke-width: 2;} 
                .parallelerPunkt text { display: none; }
                .parallelerPunkt:hover text { display: inline; stroke: rgb(0, 0, 0); stroke-width: 0.1; font-size: small; font-family: calibri}  
                """
            ]
        , g [ TypedSvg.Attributes.class [ "parallelAxis" ] ]
            [ g [ transform [ Translate (padding - 1) padding ] ] <|
                List.indexedMap
                    (\i axis ->
                        g
                            [ transform
                                [ Translate (Scale.convert xSkala (toFloat i + 1)) 0
                                ]
                            ]
                            [ axis ]
                    )
                    listeAchse
            , g [ transform [ Translate (padding - 1) 0 ] ] <|
                List.indexedMap
                    (\i desc ->
                        text_
                            [ fontFamily [ "calibri" ]
                            , fontSize (Px 12)
                            , x <| Scale.convert xSkala (toFloat i + 1)
                            , y <| padding * 7 / 8
                            , textAnchor AnchorMiddle
                            ]
                            [ TypedSvg.Core.text desc ]
                    )
                    axisLabelText
            ]
        ]
            ++ (let
                    zeichnePunkt p name beschreibung =
                        let
                            linienWeg : Path.Path
                            linienWeg =
                                List.map3
                                    (\desc s px ->
                                        Just
                                            ( Scale.convert xSkala <| toFloat desc
                                            , Scale.convert s px
                                            )
                                    )
                                    (List.range 1 (List.length axisLabelText))
                                    listeSkala
                                    p
                                    |> Shape.line Shape.linearCurve
                        in
                        g [class ["parallelerPunkt"]][
                            Path.element linienWeg
                            [ stroke <| Paint <| Color.rgba 0 0 0 0.8
                            , strokeWidth <| Px 1
                            , fill PaintNone
                            , class ["parallelerPunkt"]
                            ]
                            , text_
                                [ x 300
                                , y -20
                                , TypedSvg.Attributes.textAnchor AnchorMiddle
                                ]
                                [ TypedSvg.Core.text (name++ (String.concat<|(List.map2(\a b-> ", " ++b++ ": "++ (String.fromFloat a))p beschreibung)))]
                                
                        ]
                        
                in
                 List.map
                        (\dataset ->
                            g [ transform [ Translate (padding - 1) padding ] ]
                                (List.map (\a -> zeichnePunkt a.values a.pointName axisLabelText) dataset)
                        )  listAxisLists
               )