module DataHandling exposing (..)
import CarOfferTypes exposing (CarOffer)
import Http exposing (..)
import CarOfferTypes exposing (..)
import Html exposing (a)
import Csv.Decode exposing (..)
import Csv exposing (..)

fetchData : Cmd Msg
fetchData = Http.get
        {
            url = "https://raw.githubusercontent.com/johannesgoebel/germany-used-cars-dataset/main/Daten/data.csv"
            , expect = Http.expectString <| FetchedCSV
        }
        
dataFromCSV: String -> List CarOffer
dataFromCSV csv_string =
     Csv.parse csv_string
        |> Csv.Decode.decodeCsv decodeCarOffer
        |> Result.toMaybe
        |> Maybe.withDefault []

decodeCarOffer : Csv.Decode.Decoder (CarOffer -> a) a
decodeCarOffer =
    Csv.Decode.map CarOffer
        (Csv.Decode.field "brand" Ok
            |>  Csv.Decode.andMap (field "model" Ok)
            |>  Csv.Decode.andMap (field "color" Ok)
            |>  Csv.Decode.andMap (field "registration_date" Ok)
            |>  Csv.Decode.andMap (field "year" (String.toInt >> Result.fromMaybe "error parsing string"))
            |>  Csv.Decode.andMap (field "price_in_euro" (String.toInt >> Result.fromMaybe "error parsing string" ))
            |>  Csv.Decode.andMap (field "power_kw" (String.toInt >> Result.fromMaybe "error parsing string"  ))
            |>  Csv.Decode.andMap (field "power_ps" (String.toInt >> Result.fromMaybe "error parsing string"  ))
            |>  Csv.Decode.andMap (field "transmission_type" Ok)
            |>  Csv.Decode.andMap (field "fuel_type" Ok)
            |>  Csv.Decode.andMap (field "fuel_consumption_l_100km" (String.toFloat >> Result.fromMaybe "error parsing string"  ))
            |>  Csv.Decode.andMap (field "fuel_consumption_g_km" (String.toFloat >> Result.fromMaybe "error parsing string"  ))
            |>  Csv.Decode.andMap (field "mileage_in_km" (String.toFloat >> Result.fromMaybe "error parsing string"  ))
            |>  Csv.Decode.andMap (field "offer_description" Ok)
        )

    
-- Function to get a list of a specific attribute from a list of CarOffer
getFloatColumn : String -> List CarOffer -> List Float
getFloatColumn attribute carOffers =
    case attribute of
        "price_in_euro" ->
            List.map .price_in_euro carOffers |> List.map toFloat

        "power_kw" ->
            List.map  .power_kw carOffers |> List.map toFloat

        "power_ps" ->
            List.map .power_ps carOffers |> List.map toFloat

        "fuel_consumption_l_100km" ->
            List.map .fuel_consumption_l_100km carOffers

        "fuel_consumption_g_km" ->
            List.map .fuel_consumption_g_km carOffers

        "mileage_in_km" ->
            List.map .mileage_in_km carOffers

        _ ->
            []