module StarPlot exposing (..)
import CarOfferTypes exposing (CarOfferData)
import CarOfferTypes exposing (Msg)
import TypedSvg.Core exposing (Svg)
import TypedSvg exposing ( ..)
import TypedSvg.Types exposing ( Transform (..))
import TypedSvg.Attributes.InPx exposing (fontSize, r, x,x1, x2,y1,y2, y, cx, cy, rx,ry)
import TypedSvg.Attributes exposing (transform, viewBox)
import Html
import Scale exposing (convert, ContinuousScale)
import Color
import CarOfferTypes exposing (StarData)


-- defaultStarData : StarData
-- defaultStarData =
--     { brand = ""
--     , year = 0
--     , priceInEuro = 0.0
--     , powerKw = 0
--     , powerPs = 0
--     , fuelConsumptionLPer100km = 0.0
--     , mileageInKm = 0.0
--     , lengthOfferDescription = 0
--     }

getScaling : Float -> List Float -> List Float
getScaling radius valueList =
    let
        maxVal = List.maximum valueList |> Maybe.withDefault 0
        scaleFactor = if maxVal /= 0 then radius / maxVal else 0
    in
    List.map (\val -> val * scaleFactor) valueList

getRow : String -> List StarData -> Maybe StarData
getRow targetBrand starDataList =
    List.filter (\starData -> starData.brand == targetBrand) starDataList
        |> List.head

drawStarPlot : List StarData -> String -> Svg msg
drawStarPlot carList param =
    let
        radius = 5000.0
        angleIncrement = 2 * pi / 7
        center = { x = 0.0, y = 0.0 }

        getSpokePosition angle =
            { x = center.x + radius * cos angle
            , y = center.y + radius * sin angle
            }

        drawSpoke angle =
            let
                startPoint = center
                endPoint = getSpokePosition angle
            in
            line [ x1 startPoint.x, y1 startPoint.y, x2 endPoint.x, y2 endPoint.y ] []


        viewBoxDimensions = 10000 
    in
    svg [ viewBox 0 0 viewBoxDimensions viewBoxDimensions ]
        [ g [] (List.indexedMap (\i angle -> drawSpoke (toFloat i * angleIncrement)) (List.range 0 6)) ]
