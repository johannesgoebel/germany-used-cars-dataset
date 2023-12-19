module StarPlot exposing (..)
import CarOfferTypes exposing (CarOfferData,carOfferAttributesNumeric, Msg)
import TypedSvg.Core exposing (Svg, text)
import TypedSvg exposing ( ..)
import TypedSvg.Types exposing ( Transform (..), Paint(..), AnchorAlignment(..), px)
import TypedSvg.Attributes.InPx exposing (fontSize, r, x,x1, x2,y1,y2, y, cx, cy, rx,ry)
import TypedSvg.Attributes exposing (transform, viewBox, textAnchor)
import Scale exposing (convert, ContinuousScale)
import Color
import CarOfferTypes exposing (StarData)
import TypedSvg.Attributes exposing (stroke, strokeWidth, fill)
import TypedSvg.Attributes exposing (class)
import TypedSvg.Attributes exposing (fontFamily)

starDataToFloat : Maybe StarData -> List Float
starDataToFloat maybeStarData =
    case maybeStarData of
        Just starData ->
            [ starData.year
            , starData.price_in_euro
            , starData.power_kw
            , starData.power_ps
            , starData.fuel_consumption_l_100km
            , starData.mileage_in_km
            , starData.length_offer_description
            ]

        Nothing ->
            []

getScaling : List StarData -> List Float -> List Float
getScaling starDataList valueList =
    let
        toMaybeValues : StarData -> List (Maybe Float)
        toMaybeValues starData =
            [ Just starData.year
            , Just starData.price_in_euro
            , Just starData.power_kw
            , Just starData.power_ps
            , Just starData.fuel_consumption_l_100km
            , Just starData.mileage_in_km
            , Just starData.length_offer_description
            ]

        maxValues =
            List.foldl
                (\dataPoint acc ->
                    List.map2 (Maybe.map2 (max << abs)) (toMaybeValues dataPoint) acc
                )
                (List.repeat (List.length valueList) (Just 0))
                starDataList

        scaleFactor (val, maxValue) =
            Maybe.withDefault 0 <| Maybe.map (\maxVal -> val / maxVal) maxValue

        scaledValues =
            List.map2 (\val maybeMax -> scaleFactor (val, maybeMax)) valueList maxValues


    in
    scaledValues

getRow : String -> List StarData -> Maybe StarData
getRow targetBrand starDataList =
    List.filter (\starData -> starData.brand == targetBrand) starDataList
        |> List.head

drawStarPlot : List StarData -> String -> Svg msg
drawStarPlot carList param =
    let
        radius : Float
        radius = 2000.0
        center = { x = 3000.0, y = 3000.0 }

        getSpokePosition angle =
            { x = center.x + radius * cos angle
            , y = center.y + radius * sin angle
            }

        drawSpoke : Float -> String -> Float -> Svg msg
        drawSpoke angle label point =
            let
                startPoint = center
                endPoint = getSpokePosition angle

                pointPosition =
                    { x = startPoint.x + (endPoint.x - startPoint.x) * point
                    , y = startPoint.y + (endPoint.y - startPoint.y) * point
                    }

                tickLength = 100
                tickPositions = [0.2, 0.4, 0.6, 0.8]

                createTickAndLabel position =
                    let
                        tickEndPoint =
                            { x = startPoint.x + (endPoint.x - startPoint.x) * position
                            , y = startPoint.y + (endPoint.y - startPoint.y) * position
                            }

                        tickLabelPosition =
                            { x = startPoint.x + (endPoint.x - startPoint.x) * position
                            , y = startPoint.y + (endPoint.y - startPoint.y) * position + tickLength
                            }
                    in
                    [ line
                        [ x1 tickEndPoint.x
                        , y1 tickEndPoint.y
                        , x2 tickLabelPosition.x 
                        , y2 tickLabelPosition.y 
                        , stroke (Paint Color.black)
                        ]
                        []
                    , text_
                        [ x tickLabelPosition.x
                        , y tickLabelPosition.y
                        , textAnchor AnchorMiddle
                        , fontSize 70.0
                        ]
                        [ TypedSvg.Core.text (String.fromFloat (position * 100) ++ "%") ]
                    ]
            in
            g []
                ( List.concat
                    [ [ line
                        [ x1 startPoint.x
                        , y1 startPoint.y
                        , x2 endPoint.x
                        , y2 endPoint.y
                        , stroke (Paint Color.black)
                        ]
                        []
                    , circle
                        [ cx pointPosition.x
                        , cy pointPosition.y
                        , r 100 -- Adjust the radius of the circle as needed
                        , fill (Paint Color.red) -- Adjust the fill color of the circle
                        , fontFamily ["Bauhaus 93"]
                        ]
                        []
                    , text_
                        [ x (endPoint.x  + 500  * cos angle)  -- Adjust the x-position for centering
                        , y (endPoint.y + 500* sin angle) -- Adjust the y-position for centering
                        , textAnchor AnchorMiddle
                        , fontSize 100.0
                        , fontFamily ["Bauhaus 93"]
                        ]
                        [ TypedSvg.Core.text label ]
                    ]
                    , List.concatMap createTickAndLabel tickPositions
                    ]
                )

    
        viewBoxDimensions = 10000 
        
        rowData : Maybe StarData
        rowData = getRow param carList

        floatRowData: List Float
        floatRowData = starDataToFloat rowData

        scaledRowData: List Float
        scaledRowData = getScaling carList floatRowData

        angles =
            List.indexedMap (\i _ -> toFloat i * 2 * pi / toFloat numberOfLines) (List.range 0 (numberOfLines - 1))

        numberOfLines =
            7
        scaledFloats = [0.5, 0.4, 0.2, 0.9, 0.3, 0.4, 0.4]
        spokes : List (Svg msg)
        spokes =
            List.map3 drawSpoke  angles carOfferAttributesNumeric scaledRowData

    in
    svg [ viewBox 0 0 viewBoxDimensions viewBoxDimensions ]
    [ 
        g [] spokes
        , circle
            [ cx center.x
            , cy center.y
            , r radius
            , stroke (Paint Color.black)
            , strokeWidth (px 5)
            , fill (PaintNone)
            ]
            []
    ]

