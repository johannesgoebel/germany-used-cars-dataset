module Main exposing (..)
import CarOfferTypes exposing (..)
import DataHandling exposing (fetchData, dataFromCSV)
import Html exposing (Html, div, text, ul, li, main_)
import Browser
import Debug exposing (toString)
import List exposing (length)

main : Program () Model Msg
main
  = Browser.element
    { init = init
    , view = view
    , subscriptions = \_ -> Sub.none
    , update = update
    }

init : () -> (Model, Cmd Msg)
init _ =
  ( Loading , 
   fetchData)

update: Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        FetchedCSV result ->
            case result of
                Ok fetched_data ->
                    (Success <| {data = fetched_data}, Cmd.none)
                Err _ ->
                    (Failure, Cmd.none)


view : Model -> Html Msg
view model =
  case model of
    Failure ->
      text "I was unable to load your book."

    Loading ->
      text "Loading..."

    Success fullText ->
      main_ []
      [ 
        Html.text  fullText.data
        ]


viewCarOffers : List CarOffer -> Html Msg
viewCarOffers carOffers =
    ul []
        (List.map viewCarOffer carOffers)

viewCarOffer : CarOffer -> Html Msg
viewCarOffer carOffer =
    li []
        [ div [] [ text ("Brand: " ++ carOffer.brand) ]
        , div [] [ text ("Model: " ++ carOffer.model) ]
        , div [] [ text ("Color: " ++ carOffer.color) ]
        , div [] [ text ("Registration Date: " ++ carOffer.registration_date) ]
        , div [] [ text ("Year: " ++ String.fromInt carOffer.year) ]
        , div [] [ text ("Price in Euro: " ++ String.fromInt carOffer.price_in_euro) ]
        , div [] [ text ("Power kW: " ++ String.fromInt carOffer.power_kw) ]
        , div [] [ text ("Power PS: " ++ String.fromInt carOffer.power_ps) ]
        , div [] [ text ("Transmission Type: " ++ carOffer.transmission_type) ]
        , div [] [ text ("Fuel Type: " ++ carOffer.fuel_type) ]
        , div [] [ text ("Fuel Consumption (L/100km): " ++ String.fromFloat carOffer.fuel_consumption_l_100km) ]
        , div [] [ text ("Fuel Consumption (g/km): " ++ String.fromFloat carOffer.fuel_consumption_g_km) ]
        , div [] [ text ("Mileage (km): " ++ String.fromFloat carOffer.mileage_in_km) ]
        , div [] [ text ("Offer Description: " ++ carOffer.offer_description) ]
        ]
