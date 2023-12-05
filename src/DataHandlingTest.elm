module DataHandlingTest exposing (..)

import Browser
import Html exposing (Html, div, text)
import Http exposing (..)
import DataHandling exposing (..)


type Msg
    = FetchedCSV (Result Http.Error String)

type Model
    = Success String
    | Loading
update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case model of 
        
        case msg of
            FetchedCSV result ->
                case result of
                    Ok data ->
                        
                        ( model, Cmd.none )

                    Err err ->
                        -- Handle the error here
                        ( model, Cmd.none )


view : Model -> Html Msg
view model =
    div []
        [ text  ]


main =
    Browser.sandbox { init = model, update = update, view = view }
