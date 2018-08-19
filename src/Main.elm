module Main exposing (..)

import Html exposing (Html, text, div, h1, img, button)
import Html.Events exposing (onClick)


---- MODEL ----


type alias Model =
    { count : Int }


init : ( Model, Cmd Msg )
init =
    ( { count = 0 }, Cmd.none )



---- UPDATE ----


type Msg
    = Increment
    | Decrement


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Increment ->
            ( { model | count = model.count + 1 }, Cmd.none )

        Decrement ->
            ( { model | count = max 0 <| model.count - 1 }, Cmd.none )



---- VIEW ----


view : Model -> Html Msg
view model =
    div []
        [ h1 [] [ text <| toString model.count ]
        , button [ onClick Increment ] [ text "Increment" ]
        , button [ onClick Decrement ] [ text "Decrement" ]
        ]



---- PROGRAM ----


main : Program Never Model Msg
main =
    Html.program
        { view = view
        , init = init
        , update = update
        , subscriptions = always Sub.none
        }
