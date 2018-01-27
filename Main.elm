module Main exposing (..)

import Dict exposing (Dict)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Html.Keyed
import Json.Decode
import Random
import Task
import Time exposing (Time)


main : Program Never Model Msg
main =
    program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


-- MODEL


type alias Model =
    { gameState : GameState
    , level : Maybe Level
    , startedTime : Maybe Time
    , lastTick : Maybe Time
    , tickCount : Int
    }


initialModel : Model
initialModel =
    { gameState = Welcome
    , level = Nothing
    , startedTime = Nothing
    , lastTick = Nothing
    , tickCount = 0
    }


init : ( Model, Cmd Msg )
init =
    ( initialModel, Task.perform StartedTime Time.now )


-- MESSAGES


type GameState
    = Welcome
    | Playing
    | Results


type Msg
    = GameEnded
    | ApplyTick
    | Reset
    | StartGame
    | StartedTime Time
    | Tick Time


-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Reset ->
            ( { model | gameState = Welcome }, Cmd.none )

        StartGame ->
            let
                ( updatedState, updatedCmd ) =
                    update
                        ApplyTick
                        { model
                            | gameState = Playing
                            , level = Just Level1
                            , score = ( 0, 0, 0 )
                            , tickCount = 0
                            , superBadGuyTick = Nothing
                        }
            in
            ( updatedState, Cmd.batch [ updatedCmd, Task.perform StartedTime Time.now ] )

        StartedTime time ->
            ( { model
                | startedTime = Just time
                , superBadGuyTick = Just (randomTick (floor time))
              }
            , Cmd.none
            )

        Tick time ->
            let
                gameEnded =
                    case ( scoreToLevel model.score, model.startedTime ) of
                        ( Level1, Just startedTime ) ->
                            Time.inSeconds (time - startedTime) > 20

                        ( Level2, Just startedTime ) ->
                            Time.inSeconds (time - startedTime) > 40

                        ( Level3, Just startedTime ) ->
                            Time.inSeconds (time - startedTime) > 60

                        ( Level4, Just startedTime ) ->
                            Time.inSeconds (time - startedTime) > 80

                        _ ->
                            False
            in
            if gameEnded then
                update GameEnded model
            else
                update
                    ApplyTick
                    { model
                        | lastTick = Just time
                        , level = Just (scoreToLevel model.score)
                        , tickCount = model.tickCount + 1
                    }


-- VIEW


view : Model -> Html Msg
view model =
    case model.gameState of
        Welcome ->
            welcomeView

        Playing ->
            inGameView model

        Results ->
            resultsView model


welcomeView : Html Msg
welcomeView =
    div [ class "welcome-container" ]
        [ h1 [] [ text "Welcome to Whack-a-Fraudster" ]
        , button [ class "start", onClick StartGame ] [ text "Start" ]
        ]


subscriptions : Model -> Sub Msg
subscriptions model =
    let
        interval =
            case model.level of
                Just Level1 ->
                    Time.second * 2

                Just Level2 ->
                    Time.second * 1.5

                Just Level3 ->
                    Time.second * 1

                _ ->
                    Time.second * 0.75
    in
    case model.gameState of
        Playing ->
            Time.every interval Tick


        _ ->
            Sub.none


