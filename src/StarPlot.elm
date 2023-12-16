module StarPlot exposing (..)
import CarOfferTypes exposing (CarOfferData,carOfferAttributesNumeric, Msg)
import TypedSvg.Core exposing (Svg)
import TypedSvg exposing ( ..)
import TypedSvg.Types exposing ( Transform (..), Paint(..), AnchorAlignment(..))
import TypedSvg.Attributes.InPx exposing (fontSize, r, x,x1, x2,y1,y2, y, cx, cy, rx,ry)
import TypedSvg.Attributes exposing (transform, viewBox, textAnchor)
import Html
import Scale exposing (convert, ContinuousScale)
import Color
import CarOfferTypes exposing (StarData)
import DataHandling exposing (log)
import TypedSvg.Attributes exposing (stroke, strokeWidth, fill)

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
        |> log "Roe" << identity

drawStarPlot : List StarData -> String -> Svg msg
drawStarPlot carList param =
    let
        radius : Float
        radius = 1000.0
        center = { x = 3000.0, y = 2000.0 }

        getSpokePosition angle =
            { x = center.x + radius * cos angle
            , y = center.y + radius * sin angle
            }

        drawSpoke: Float -> String -> Float -> Svg msg
        drawSpoke angle label point=
            let
                startPoint = center
                endPoint = getSpokePosition angle

                pointPosition =
                    { x = startPoint.x + (endPoint.x - startPoint.x) * point
                    , y = startPoint.y + (endPoint.y - startPoint.y) * point
                    }
            in
            g []
            [ line
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
                , r 5 -- Adjust the radius of the circle as needed
                , fill (Paint Color.red) -- Adjust the fill color of the circle
                ]
                []
            , text_
                [ x (endPoint.x + (startPoint.x - endPoint.x) / 2 +50)  -- Adjust the x-position for centering
                , y (endPoint.y + (startPoint.y - endPoint.y) / 2 +50) -- Adjust the y-position for centering
                , textAnchor AnchorMiddle
                ]
                [ TypedSvg.Core.text label ]
            ]
            
        viewBoxDimensions = 10000 
        
        rowData : Maybe StarData
        rowData = getRow param carList

        floatRowData: List Float
        floatRowData = starDataToFloat rowData

        scaledRowData: List Float
        scaledRowData = getScaling carList floatRowData
            |> Debug.log "scaled values"

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
    [ g [] spokes
    , circle
        [ cx center.x
        , cy center.y
        , r radius
        , stroke (Paint Color.black)
        , fill (PaintNone)
        ]
        []
    ]

