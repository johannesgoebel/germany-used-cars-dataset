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
import Debug exposing (toString)


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

drawScatterplot: List String -> List Float -> List Float -> List String -> String -> String -> Svg msg 
drawScatterplot pointLabels xValues yValues transmissionList xLabel yLabel =
    let
        points : List (Float, Float)
        points =
            List.map2 (\x y-> (x,y))   xValues yValues
            
                
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
            Axis.bottom [ Axis.tickCount tickCount ] (xScale values )

        

        yAxis : List Float -> Svg msg
        yAxis values =
            Axis.left [ Axis.tickCount tickCount ] (yScale values )   
            
        wideExtent : List Float -> ( Float, Float )
        wideExtent values =
            Statistics.extent ( values) |> Maybe.withDefault (0,0)|> Tuple.mapFirst (\x -> x - (Maybe.withDefault 0 (List.maximum values)) / 20) |> Tuple.mapSecond (\x -> x +  Maybe.withDefault 0 (List.maximum values) / 10)
          
        getClassByTransmission: String -> String
        getClassByTransmission transmission = 
            case transmission of
                "Petrol" -> 
                    "petrol"   
                "Diesel" ->
                    "diesel"           
                "Hybrid" ->
                    "hybrid"                
                "Electric" ->
                    "electric"                
                "LPG" ->
                      "lpg"              
                "CNG"               ->
                        "cng"            
                "Diesel Hybrid"     ->
                        "diesel_hybrid"           
                "Other"             ->
                         "other"           
                "Unknown"           ->
                          "unknown"          
                "Hydrogen"          ->
                          "hydrogen"         
                "Ethanol"           ->
                          "ethanol"  
                _ ->
                    "unknown"

        makePoint : ContinuousScale Float -> ContinuousScale Float -> (Float, Float) -> String ->  String -> Svg msg
        makePoint scaleX scaleY point pointLabelText transmission=
            let
                classForTransmission : String
                classForTransmission = getClassByTransmission transmission
            in
            
            g [ class [ classForTransmission ], fontSize <| Px 10.0, fontFamily [ "sans-serif" ] ]       
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
                    [ text (pointLabelText )]
                ]
              
    in            
    
    svg [ viewBox 0 0 w h, TypedSvg.Attributes.width <| TypedSvg.Types.Percent 100, TypedSvg.Attributes.height <| TypedSvg.Types.Percent 100 ]
        [ style [] [ TypedSvg.Core.text css_text ]
        , g
            [ transform [ Translate (padding - 1) ( padding - 1 ) ]
            , fontSize <| Px 10.0
            , fontFamily [ "sans-serif" ]
            , class [ "r", "point" ]
            ]
            []
         -- labels    
        , g[] [text_ [ TypedSvg.Attributes.InPx.fontSize 10 , y 10, x 200] [ text ("Hybrid")]
                , text_ [ TypedSvg.Attributes.InPx.fontSize 10 , y 25, x 200] [ text ("Electric")]
                , text_ [ TypedSvg.Attributes.InPx.fontSize 10 , y 10, x 350] [ text ("Lpg")]
                , text_ [ TypedSvg.Attributes.InPx.fontSize 10 , y 25, x 350] [ text ("Cng")]
                , text_ [ TypedSvg.Attributes.InPx.fontSize 10 , y 10, x 500] [ text ("Diesel Hybrid")]
                , text_ [ TypedSvg.Attributes.InPx.fontSize 10 , y 25, x 500] [ text ("Other")]
                , text_ [ TypedSvg.Attributes.InPx.fontSize 10 , y 10, x 100] [ text ("Petrol")]
                , text_ [ TypedSvg.Attributes.InPx.fontSize 10 , y 25, x 100] [ text ("Diesel")]
                , text_ [ TypedSvg.Attributes.InPx.fontSize 10 , y 10, x 610] [ text ("Unknown")]
                , text_ [ TypedSvg.Attributes.InPx.fontSize 10 , y 25, x 610] [ text ("Ethanol")]
                ]    
         -- colors        
        , g[class [ "petrol" ]][rect  [ x 85 , y 3, width 10, height 10][]]
        , g[class [ "diesel" ]][rect  [ x 85 , y 18, width 10, height 10][]]
        , g[class [ "hybrid" ]][rect  [ x 185 , y 3, width 10, height 10][]]       
        , g[class [ "electric" ]][rect  [ x 185 , y 18, width 10, height 10][]]
        , g[class [ "lpg" ]][rect  [ x 335 , y 3, width 10, height 10][]]    
        , g[class [ "cng" ]][rect  [ x 335 , y 18, width 10, height 10][]]    
        , g[class [ "diesel_hybrid" ]][rect  [ x 485 , y 3, width 10, height 10][]]    
        , g[class [ "other" ]][rect  [ x 485 , y 18, width 10, height 10][]]    
        , g[class [ "unknown" ]][rect  [ x 595 , y 3, width 10, height 10][]]    
        , g[class [ "ethanol" ]][rect  [ x 595 , y 18, width 10, height 10][]]    
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
                (List.map3 (makePoint xScaleLocal yScaleLocal)  points pointLabels transmissionList  )
        ]


css_text : String
css_text =
    """
                .point circle { stroke: rgba(0, 0, 0,0.4); fill: rgba(255, 255, 255,0.3); }
                .point text { display: none; }
                .point:hover circle { stroke: rgba(0, 0, 0,1.0); fill: rgb(118, 214, 78); }
                

                .petrol circle { stroke: rgba(0, 0, 0,0.4); fill:rgb(255, 0, 0); }
                .petrol rect { stroke: rgba(0, 0, 0,0.4); fill: rgb(255, 0, 0); }
                .petrol text { display: none; }

                .diesel circle { stroke: rgba(0, 0, 0,0.4); fill:rgb(0, 255, 0); }
                .diesel rect { stroke: rgba(0, 0, 0,0.4); fill:rgb(0, 255, 0); }
                .diesel text { display: none; }

                .hybrid circle { stroke: rgba(0, 0, 0,0.4); fill:rgb(0, 0, 255) ; }
                .hybrid rect { stroke: rgba(0, 0, 0,0.4); fill: rgb(0, 0, 255) ; }
                .hybrid text { display: none; }

                .electric circle { stroke: rgba(0, 0, 0,0.4); fill:rgb(255, 255, 0) ; }
                .electric rect { stroke: rgba(0, 0, 0,0.4); fill:rgb(255, 255, 0) ; }
                .electric text { display: none; }

                .lpg circle { stroke: rgba(0, 0, 0,0.4); fill: rgb(255, 0, 255) ; }
                .lpg rect { stroke: rgba(0, 0, 0,0.4); fill: rgb(255, 0, 255) ; }
                .lpg text { display: none; }

                .cng circle { stroke: rgba(0, 0, 0,0.4); fill:rgb(0, 255, 255); }
                .cng rect { stroke: rgba(0, 0, 0,0.4); fill: rgb(0, 255, 255); }
                .cng text { display: none; }

                .diesel_hybrid circle { stroke: rgba(0, 0, 0,0.4); fill: rgb(128, 0, 128); }
                .diesel_hybrid rect { stroke: rgba(0, 0, 0,0.4); fill: rgb(128, 0, 128); }
                .diesel_hybrid text { display: none; }

                .other circle { stroke: rgba(0, 0, 0,0.4); fill: rgb(255, 165, 0); }
                .other rect { stroke: rgba(0, 0, 0,0.4); fill: rgb(255, 165, 0); }
                .other text { display: none; }

                .unknown circle { stroke: rgba(0, 0, 0,0.4); fill: rgb(128, 128, 128) ; }
                .unknown rect { stroke: rgba(0, 0, 0,0.4); fill: rgb(128, 128, 128) ; }
                .unknown text { display: none; }

                .hydrogen circle { stroke: rgba(0, 0, 0,0.4); fill: rgb(0, 128, 0) ; }
                .hydrogen rect { stroke: rgba(0, 0, 0,0.4); fill: rgb(0, 128, 0) ; }
                .hydrogen text { display: none;}

                .ethanol circle { stroke: rgba(0, 0, 0,0.4); fill: rgb(128, 0, 0); }
                .ethanol rect { stroke: rgba(0, 0, 0,0.4); fill: rgb(128, 0, 0); }
                .ethanol text { display: none; }

    """