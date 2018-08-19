module Main exposing (..)

import Html exposing (Html, text, div, h1, img, button, p, input)
import Html.Events exposing (onClick, onInput)
import Html.Attributes exposing (value, style)
import Time exposing (Time, second)


---- MODEL ----


type TimerDirection
    = Up
    | Down


type alias Model =
    { count : Int
    , timerActive : Bool
    , direction : TimerDirection
    }


init : ( Model, Cmd Msg )
init =
    ( { count = 0, timerActive = False, direction = Down }, Cmd.none )



---- UPDATE ----


type Msg
    = Increment
    | Decrement
    | SetCount (Result String Int) -- Result error value
    | ToggleTimer
    | ToggleDirection
    | Tick Time


updateCount : Model -> Model
updateCount model =
    case model.direction of
        Up ->
            { model | count = model.count + 1 }

        Down ->
            { model | count = max 0 <| model.count - 1 }


toggleDirection : Model -> Model
toggleDirection model =
    case model.direction of
        Up ->
            { model | direction = Down }

        Down ->
            { model | direction = Up }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Increment ->
            ( { model | count = model.count + 1 }, Cmd.none )

        Decrement ->
            ( { model | count = max 0 <| model.count - 1 }, Cmd.none )

        ToggleTimer ->
            ( { model | timerActive = not model.timerActive }, Cmd.none )

        ToggleDirection ->
            ( toggleDirection model, Cmd.none )

        SetCount (Ok newCount) ->
            ( { model | count = newCount }, Cmd.none )

        SetCount (Err err) ->
            ( model, Cmd.none )

        -- SetCount result ->
        --     case result of
        --         Ok newCount ->
        --             ( { model | count = newCount }, Cmd.none )
        --         Err error ->
        --             ( model, Cmd.none )
        Tick _ ->
            ( updateCount model, Cmd.none )



---- VIEW ----


view : Model -> Html Msg
view model =
    div []
        [ input
            [ value <| toString model.count
            , onInput <| SetCount << String.toInt -- , onInput (\value -> SetCount (String.toInt value))
            ]
            []
        , p [] [ text <| "Timer: " ++ toString model.timerActive ]
        , p [] [ text <| "Direction: " ++ toString model.direction ]
        , button [ onClick Increment ] [ text "Increment" ]
        , button [ onClick Decrement ] [ text "Decrement" ]
        , button [ onClick ToggleTimer ] [ text "Toggle Timer" ]
        , button [ onClick ToggleDirection ] [ text "Toggle Direction" ]
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
