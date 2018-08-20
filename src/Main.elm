module Main exposing (..)

import Html exposing (Html, text, div, h1, img, button, p, input)
import Html.Events exposing (onClick, onInput)
import Html.Attributes exposing (value, style, disabled)
import Time exposing (Time, second)


---- MODEL ----


type TimerDirection
    = Up
    | Down


type TimerStatus
    = Off
    | On TimerDirection


type alias Model =
    { count : Int
    , status : TimerStatus
    }


init : ( Model, Cmd Msg )
init =
    ( { count = 0
      , status = Off
      }
    , Cmd.none
    )



---- UPDATE ----


type Msg
    = Increment
    | Decrement
    | SetCount (Result String Int) -- Result error value
    | ToggleTimer
    | ToggleDirection
    | Tick Time


updateCount : TimerDirection -> Int -> Int
updateCount direction count =
    case direction of
        Up ->
            count + 1

        Down ->
            max 0 <| count - 1


increment : Int -> Int
increment =
    updateCount Up


decrement : Int -> Int
decrement =
    updateCount Down


toggleDirection : TimerDirection -> Model -> Model
toggleDirection direction model =
    case direction of
        Up ->
            { model | status = On Down }

        Down ->
            { model | status = On Up }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Increment ->
            ( { model | count = (increment model.count) }, Cmd.none )

        Decrement ->
            ( { model | count = (decrement model.count) }, Cmd.none )

        ToggleTimer ->
            case model.status of
                Off ->
                    ( { model | status = On Down }, Cmd.none )

                On _ ->
                    ( { model | status = Off }, Cmd.none )

        ToggleDirection ->
            case model.status of
                Off ->
                    ( model, Cmd.none )

                On direction ->
                    ( model |> toggleDirection direction, Cmd.none )

        SetCount (Ok newCount) ->
            ( { model
                | count = newCount
              }
            , Cmd.none
            )

        SetCount (Err err) ->
            model ! []

        -- SetCount result ->
        --     case result of
        --         Ok newCount ->
        --             ( { model | count = newCount }, Cmd.none )
        --         Err error ->
        --             ( model, Cmd.none )
        Tick _ ->
            case model.status of
                Off ->
                    -- (model, Cmd.none)
                    model ! []

                On direction ->
                    ( { model | count = (updateCount direction model.count) }, Cmd.none )



---- VIEW ----


view : Model -> Html Msg
view model =
    div []
        [ input
            [ value <| toString model.count
            , disabled (model.status /= Off)
            , onInput <| SetCount << String.toInt -- , onInput (\value -> SetCount (String.toInt value))
            ]
            []
        , p [] [ text <| "Timer: " ++ toString model.status ]
        , button [ onClick Increment ] [ text "+" ]
        , button [ onClick Decrement ] [ text "-" ]
        , button [ onClick ToggleTimer ]
            [ text <|
                if model.status == Off then
                    "Start"
                else
                    "Stop"
            ]
        , button
            [ onClick ToggleDirection
            , disabled (model.status == Off)
            ]
            [ text "Toggle Direction" ]
        ]



---- SUBSCRIPTIONS ----


subscriptions : Model -> Sub Msg
subscriptions model =
    case model.status of
        Off ->
            Sub.none

        On _ ->
            Time.every second Tick



---- PROGRAM ----


main : Program Never Model Msg
main =
    Html.program
        { view = view
        , init = init
        , update = update
        , subscriptions = subscriptions
        }
