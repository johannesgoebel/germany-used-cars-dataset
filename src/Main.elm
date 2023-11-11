module Main exposing (..)
import CarOfferTypes exposing (carOfferAttributes, Model, Msg, CarOffer)
import Scatterplot exposing (drawScatterplot)
import DataHandling exposing (fetchData, dataFromCSV)
import Html exposing (Html, div, text, ul, li, main_, h1, h2, p)
import Html.Events exposing (..)
import Html.Attributes exposing (..)
import Browser
import CarOfferTypes exposing (Model(..))
import CarOfferTypes exposing (carOfferAttributesNumeric)
import DataHandling exposing (getFloatColumn)
import ParallelPlot exposing (drawParallelplot)
import DataHandling exposing (generateParallelAxisCarOffers)
import Bootstrap.CDN as CDN
import Bootstrap.Grid as Grid


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
  ( CarOfferTypes.Loading , 
   fetchData)

update: Msg -> Model -> (Model, Cmd Msg)
update msg model=
    case msg of
        CarOfferTypes.FetchedCSV result ->
            case result of
                Ok fetched_data ->
                    (CarOfferTypes.Success <| {data = (DataHandling.dataFromCSV fetched_data)
                                              ,yAxis = "model"
                                              ,xAxis ="price_in_euro"
                                              ,firstCoordinate = "price_in_euro"
                                              ,secondCoordinate = "power_ps" 
                                              ,thirdCoordinate = "fuel_consumption_g_km"
                                              ,forthCoordinate = "mileage_in_km"}, Cmd.none)
                Err _ ->
                    (CarOfferTypes.Failure, Cmd.none)
        CarOfferTypes.SelectChangeYScatterplot yUpdate -> 
          case model of
              Success d ->
                (Success <| {d | yAxis = yUpdate}, Cmd.none)
              _ -> 
                (model, Cmd.none)
        CarOfferTypes.SelectChangeXScatterplot xUpdate -> 
          case model of
              Success d ->
                (Success <| {d | xAxis = xUpdate}, Cmd.none)
              _ -> 
                (model, Cmd.none)
        
        CarOfferTypes.SelectChange1PolarPlot firstUpdate ->
          case model of
              Success d ->
                (Success <| {d | firstCoordinate = firstUpdate}, Cmd.none)
              _ -> 
                (model, Cmd.none)
        CarOfferTypes.SelectChange2PolarPlot secondUpdate -> 
          case model of
              Success d ->
                (Success <| {d | secondCoordinate = secondUpdate}, Cmd.none)
              _ -> 
                (model, Cmd.none)
        CarOfferTypes.SelectChange3PolarPlot thirdUpdate-> 
          case model of
              Success d ->
                (Success <| {d | thirdCoordinate = thirdUpdate}, Cmd.none)
              _ -> 
                (model, Cmd.none)
        CarOfferTypes.SelectChange4PolarPlot forthUpdate -> 
          case model of
              Success d ->
                (Success <| {d | forthCoordinate = forthUpdate}, Cmd.none)
              _ -> 
                (model, Cmd.none)

view : Model -> Html Msg
view model =
  case model of
    CarOfferTypes.Failure ->
      text "I was unable to load your book."

    CarOfferTypes.Loading ->
      text "Loading..."

    CarOfferTypes.Success fullText ->
      main_ []         -- Responsive fixed width container
        [ topText
          , scatterPlotText
          , div [class "row"] -- Add Bootstrap row class
              [ div [class "col-md-6"] [ -- Use Bootstrap col-md-6 class for half-width
                    div [] [ Html.p [] [ Html.text "Adjust attribute shown on x-coordinate." ]
                        , viewDropdown carOfferAttributesNumeric CarOfferTypes.SelectChangeXScatterplot
                    ]
                ]
              , div [class "col-md-6"] [ -- Use Bootstrap col-md-6 class for half-width
                    div [] [ Html.p [] [ Html.text "Adjust attribute shown on y-coordinate." ]
                        , viewDropdown carOfferAttributesNumeric CarOfferTypes.SelectChangeYScatterplot
                    ]
                ]
              ]
          , drawScatterplot (List.map .offer_description fullText.data) (getFloatColumn fullText.xAxis fullText.data) (getFloatColumn fullText.yAxis fullText.data) fullText.xAxis fullText.yAxis
          , paralellPlotText
          , div [class "row"] -- Add Bootstrap row class
              [ div [class "col-md-3"] [
                    div [] [ Html.p [] [ Html.text "Adjust attribute shown on first coordinate." ]
                        , viewDropdown carOfferAttributesNumeric CarOfferTypes.SelectChange1PolarPlot
                    ]
                ]
              , div [class "col-md-3"] [
                    div [] [ Html.p [] [ Html.text "Adjust attribute shown on second coordinate." ]
                        , viewDropdown carOfferAttributesNumeric CarOfferTypes.SelectChange2PolarPlot
                    ]
                ]
              , div [class "col-md-3"] [
                    div [] [ Html.p [] [ Html.text "Adjust attribute shown on third coordinate." ]
                        , viewDropdown carOfferAttributesNumeric CarOfferTypes.SelectChange3PolarPlot
                    ]
                ]
              , div [class "col-md-3"] [
                    div [] [ Html.p [] [ Html.text "Adjust attribute shown on fourth coordinate." ]
                        , viewDropdown carOfferAttributesNumeric CarOfferTypes.SelectChange4PolarPlot
                    ]
                ]
              ]
          , drawParallelplot (generateParallelAxisCarOffers fullText.data fullText.firstCoordinate fullText.secondCoordinate fullText.thirdCoordinate fullText.forthCoordinate) fullText.firstCoordinate fullText.secondCoordinate fullText.thirdCoordinate fullText.forthCoordinate
          ]


viewCarOffers : List CarOffer -> Html Msg
viewCarOffers carOffers =
  ul [] (List.map viewCarOffer carOffers)
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
        
viewDropdown : List String -> (String -> Msg) -> Html Msg
viewDropdown options onInputMsg =
    div []
        [ Html.select [ Html.Events.onInput onInputMsg ]
            (List.map (\opt -> Html.option [ Html.Attributes.value opt ] [ text opt ]) options)
        ]

topText: Html Msg
topText =
  div[]
  [
    h1 [] [text "Used Car Offers"],
    p []  [text "Welcome to this GitHub repository showcasing the outcome of a Information Visualization project. This project comprises three visualizations, offering insights into the world of used car offers extracted from a prominent online marketplace. The goal of this project is to provide valuable perspectives and actionable intelligence for stakeholders in the automotive industry."
    ]
  ]

scatterPlotText: Html Msg
scatterPlotText =
  div[]
  [
    h2 [] [text "Compare the attributes of used cars"]
    ,p []  [text "Below, you'll find our scatterplot, a powerful tool for comparing various attributes of used car offers. This visualization enables you to analyze and discern patterns, correlations, and disparities among key features of the listings, offering valuable insights into the diverse landscape of available vehicles."
    ]
  ]

paralellPlotText: Html Msg
paralellPlotText =
  div[]
  [
    h2 [] [text "Parallel Plot"]
    ,p []  [text "Below, you'll find our parallel plot, a powerful tool for comparing various attributes of used car offers. This visualization enables you to analyze and discern patterns, correlations, and disparities among key features of the listings, offering valuable insights into the diverse landscape of available vehicles."
    ]
  ]