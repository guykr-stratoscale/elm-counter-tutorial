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


updateCount : Int -> TimerDirection -> Int
updateCount count direction =
    case direction of
        Up ->
            count + 1

        Down ->
            max 0 <| count - 1


toggleDirection : Model -> TimerDirection -> Model
toggleDirection model direction =
    case direction of
        Up ->
            { model | status = On Down }

        Down ->
            { model | status = On Up }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Increment ->
            ( { model | count = (updateCount model.count Up) }, Cmd.none )

        Decrement ->
            ( { model | count = (updateCount model.count Down) }, Cmd.none )

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
                    ( toggleDirection model direction, Cmd.none )

        SetCount (Ok newCount) ->
            ( { model
                | count = newCount
                , status = On Down
              }
            , Cmd.none
            )

        SetCount (Err err) ->
            ( model, Cmd.none )

        -- SetCount result ->
        --     case result of
        --         Ok newCount ->
        --             ( { model | count = newCount }, Cmd.none )
        --         Err error ->
        --             ( model, Cmd.none )
        Tick _ ->
            case model.status of
                Off ->
                    ( model, Cmd.none )

                On direction ->
                    ( { model | count = (updateCount model.count direction) }, Cmd.none )



---- VIEW ----


view : Model -> Html Msg
view model =
    div []
        [ input
            [ value <| toString model.count
            , onInput <| SetCount << String.toInt -- , onInput (\value -> SetCount (String.toInt value))
            ]
            []
        , p [] [ text <| "Timer: " ++ toString model.status ]
        , button [ onClick Increment ] [ text "Increment" ]
        , button [ onClick Decrement ] [ text "Decrement" ]
        , button [ onClick ToggleTimer ] [ text "Toggle Timer" ]
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
