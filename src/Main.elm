module Main exposing (..)

import Html exposing (Html, text, div, h1, img, button, h3)
import Html.Events exposing (onClick)
import Time exposing (Time, second)


---- MODEL ----


type alias Model =
    { count : Int, timerActive : Bool }


init : ( Model, Cmd Msg )
init =
    ( { count = 0, timerActive = False }, Cmd.none )



---- UPDATE ----


type Msg
    = Increment
    | Decrement
    | TimerToggle
    | Tick Time


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Increment ->
            ( { model | count = model.count + 1 }, Cmd.none )

        Decrement ->
            ( { model | count = max 0 <| model.count - 1 }, Cmd.none )

        TimerToggle ->
            ( { model | timerActive = not model.timerActive }, Cmd.none )

        Tick _ ->
            ( { model | count = model.count + 1 }, Cmd.none )



---- VIEW ----


view : Model -> Html Msg
view model =
    div []
        [ h1 [] [ text <| toString model.count ]
        , h3 [] [ text <| "Timer: " ++ toString model.timerActive ]
        , button [ onClick Increment ] [ text "Increment" ]
        , button [ onClick Decrement ] [ text "Decrement" ]
        , button [ onClick TimerToggle ] [ text "Toggle Timer" ]
        ]



---- SUBSCRIPTIONS ----


subscriptions : Model -> Sub Msg
subscriptions model =
    if model.timerActive then
        Time.every second Tick
    else
        Sub.none



---- PROGRAM ----


main : Program Never Model Msg
main =
    Html.program
        { view = view
        , init = init
        , update = update
        , subscriptions = subscriptions
        }
