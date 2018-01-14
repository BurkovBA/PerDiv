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
    { bonus : Bool
    , gameState : GameState
    , gridContents : Dict Int ContentType
    , level : Maybe Level
    , randomSequence : List Int
    , superBadGuyTick : Maybe Int
    , score : ( Int, Int, Int )
    , startedTime : Maybe Time
    , lastTick : Maybe Time
    , tickCount : Int
    }


initialModel : Model
initialModel =
    { bonus = False
    , gameState = Welcome
    , gridContents = Dict.empty
    , level = Nothing
    , randomSequence = []
    , superBadGuyTick = Nothing
    , score = ( 0, 0, 0 )
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


type Level
    = Level1
    | Level2
    | Level3
    | Level4


type ContentType
    = Empty
    | Fraudster
    | SuperFraudster
    | Client


type Msg
    = ClickBox Int
    | GameEnded
    | ApplyTick
    | Reset
    | StartGame
    | StartedTime Time
    | Tick Time




-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ApplyTick ->
            let
                superBadGuyRemains =
                    getSuperBadGuyFromGrid model.superBadGuyTick model.tickCount model.gridContents

                ( numberOfClients, numberOfFraudsters, rows ) =
                    getLevelConfig model.level

                emptySpaces =
                    if List.isEmpty superBadGuyRemains then
                        (rows * rows) - 1
                    else
                        (rows * rows) - 2

                cellContentList =
                    createContentList emptySpaces numberOfFraudsters numberOfClients (isSuperBadGuyTick model.superBadGuyTick model.tickCount)

                randomNumberList =
                    case ( model.lastTick, model.startedTime ) of
                        ( Just time, _ ) ->
                            randomList (floor time) emptySpaces

                        ( _, Just time ) ->
                            randomList (floor time) emptySpaces

                        _ ->
                            []

                joinedLists =
                    List.map2 (\content randomNumber -> ( content, randomNumber )) cellContentList randomNumberList
                        |> List.sortBy (\( _, randomNumber ) -> randomNumber)
                        |> List.indexedMap
                            (\index ( cellType, _ ) ->
                                if index >= floor ((toFloat rows * toFloat rows) / 2) then
                                    ( index + 1, cellType )
                                else
                                    ( index, cellType )
                            )

                gridContents =
                    adjustListsWithSuperBadGuy rows joinedLists superBadGuyRemains
                        |> Dict.fromList
            in
            ( { model | gridContents = gridContents }, Cmd.none )

        ClickBox index ->
            let
                ( fraudsters, customers, superbadGuy ) =
                    model.score
            in
            case Dict.get index model.gridContents of
                Just SuperFraudster ->
                    ( { model
                        | score = ( fraudsters, customers, superbadGuy + 1 )
                        , gridContents =
                            Dict.update
                                index
                                (Maybe.map (\previousContentTypes -> Empty))
                                model.gridContents
                      }
                    , Cmd.none
                    )

                Just Fraudster ->
                    ( { model
                        | score = ( fraudsters + 1, customers, superbadGuy )
                        , gridContents =
                            Dict.update
                                index
                                (Maybe.map (\previousContentTypes -> Empty))
                                model.gridContents
                      }
                    , Cmd.none
                    )

                Just Client ->
                    ( { model
                        | score = ( fraudsters, customers + 1, superbadGuy )
                        , gridContents =
                            Dict.update
                                index
                                (Maybe.map (\previousContentTypes -> Empty))
                                model.gridContents
                      }
                    , Cmd.none
                    )

                _ ->
                    ( model, Cmd.none )


        GameEnded ->
            ( { model | gameState = Results }, Cmd.none )




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


                        const a = 0

                        a  funcA()

                        b = a + 1


                        model = 2

                        { model | score = 1 }
                        model
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

-- ApplyTick Helpers


getLevelConfig : Maybe Level -> ( Int, Int, Int )
getLevelConfig level =
    case level of
        Just Level1 ->
            ( 2, 1, 3 )

        Just Level2 ->
            ( 4, 2, 5 )

        Just Level3 ->
            ( 6, 3, 7 )

        Just Level4 ->
            ( 8, 5, 9 )

        Nothing ->
            ( 0, 0, 0 )

isSuperBadGuyTick : Maybe Int -> Int -> Bool
isSuperBadGuyTick superBadGuyTick tickCount =
    case superBadGuyTick of
        Just superBadGuyTick ->
            superBadGuyTick == tickCount

        Nothing ->
            False


getSuperBadGuyFromGrid : Maybe Int -> Int -> Dict Int ContentType -> List ( Int, ContentType )
getSuperBadGuyFromGrid superBadGuyTick tickCount gridContents =
    case superBadGuyTick of
        Just superBadGuyTick ->
            if (superBadGuyTick + 2) >= tickCount then
                Dict.toList gridContents
                    |> List.filter
                        (\( index, cellType ) ->
                            cellType == SuperFraudster
                        )
            else
                []

        Nothing ->
            []





adjustListsWithSuperBadGuy : Int -> List ( Int, ContentType ) -> List ( Int, ContentType ) -> List ( Int, ContentType )
adjustListsWithSuperBadGuy rows joinedLists superBadGuyRemains =
    case List.head superBadGuyRemains of
        Just ( index, _ ) ->
            let
                adjustedIndex =
                    if index >= floor ((toFloat rows * toFloat rows) / 2) then
                        index - 1
                    else
                        index

                beforeSuperBadGuy =
                    List.take adjustedIndex joinedLists

                afterSuperBadGuy =
                    List.drop adjustedIndex joinedLists
            in
            beforeSuperBadGuy
                ++ [ ( index, SuperFraudster ) ]
                ++ afterSuperBadGuy
                |> List.indexedMap
                    (\index ( _, cellType ) ->
                        if index >= floor ((toFloat rows * toFloat rows) / 2) then
                            ( index + 1, cellType )
                        else
                            ( index, cellType )
                    )

        Nothing ->
            joinedLists



createContentList : Int -> Int -> Int -> Bool -> List ContentType
createContentList emptySpaces numberOfFraudsters numberOfClients isSuperBadGuyTick =
    let
        fraudsters =
            List.range 1 numberOfFraudsters
                |> List.map (\_ -> Fraudster)

        clients =
            List.range 1 numberOfClients
                |> List.map (\_ -> Client)

        superbadGuy =
            if isSuperBadGuyTick then
                [ SuperFraudster ]
            else
                []

        empties =
            List.range 1 (emptySpaces - numberOfFraudsters - numberOfClients)
                |> List.map (\_ -> Empty)
    in
    fraudsters
        ++ clients
        ++ superbadGuy
        ++ (if isSuperBadGuyTick then
                List.take (List.length empties - 1) empties
            else
                empties
           )


insertContentType : Int -> ContentType -> Dict Int ContentType -> Dict Int ContentType
insertContentType index contentType gridContents =
    Dict.insert index contentType gridContents



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


inGameView : Model -> Html Msg
inGameView model =
    let
        ( levelClass, grid ) =
            case model.level of
                Just Level1 ->
                    ( [ class "grid-container--level1" ], makeGrid 3 model.gridContents )

                Just Level2 ->
                    ( [ class "grid-container--level2" ], makeGrid 5 model.gridContents )

                Just Level3 ->
                    ( [ class "grid-container--level3" ], makeGrid 7 model.gridContents )

                Just Level4 ->
                    ( [ class "grid-container--level4" ], makeGrid 9 model.gridContents )

                Nothing ->
                    ( [], [] )
    in
    div []
        [ div [ class "score" ] [ span [] [ text "Score: " ], span [] [ text (toString (translateScore model.score)) ] ]
        , button [ class "reset", onClick Reset ] [ div [] [ text "X" ], span [] [ text "Reset" ] ]
        , div ([ class "grid-container" ] ++ levelClass) grid
        , button [ class "end-game", onClick GameEnded ] [ text "End Game" ]
        ]


resultsView : Model -> Html Msg
resultsView model =
    let
        ( fraudstersPercentage, customersPercentage, superbadGuyPercentage ) =
            scoreToPercentage model.score

        ( fraudsters, customers, superbadGuy ) =
            model.score
    in
    div []
        [ div [ class "score" ] [ span [] [ text "Score: " ], span [] [ text (toString (translateScore model.score)) ] ]
        , button [ class "reset", onClick Reset ] [ div [] [ text "X" ], span [] [ text "Reset" ] ]
        , div [ class "result-graph" ]
            [ div [ class "left-side" ]
                [ div [ class "label" ] [ text "Fraudsters" ]
                , div
                    [ class "bar"
                    , title ("- " ++ toString (customers * 100) ++ " points")
                    , style
                        [ ( "width", toString customersPercentage ++ "%" )
                        , ( "margin-left", toString (100 - customersPercentage) ++ "%" )
                        ]
                    ]
                    []
                , div [ class "label" ] [ text "Super Fraudster" ]
                ]
            , div [ class "seperator" ] []
            , div [ class "right-side" ]
                [ div
                    [ class "bar"
                    , title (toString (fraudsters * 50) ++ " points")
                    , style [ ( "width", toString fraudstersPercentage ++ "%" ) ]
                    ]
                    []
                , div [ class "label" ] [ text "Customers" ]
                , div
                    [ class "bar"
                    , title (toString (superbadGuy * 250) ++ " points")
                    , style [ ( "width", toString superbadGuyPercentage ++ "%" ) ]
                    ]
                    []
                ]
            ]
        , div [ class "playing-time" ] [ text (calculatePlayingTime model.lastTick model.startedTime) ]
        ]


scoreToPercentage : ( Int, Int, Int ) -> ( Float, Float, Float )
scoreToPercentage ( fraudsters, customers, superbadGuy ) =
    let
        fraudstersScore =
            fraudsters * 50

        customersScore =
            customers * 100

        superbadGuyScore =
            superbadGuy * 250

        total =
            fraudstersScore + customersScore + superbadGuyScore
    in
    ( if total == 0 && fraudsters == 0 && customers == 0 then
        0
      else
        (toFloat fraudstersScore / toFloat total) * 100
    , if total == 0 && fraudsters == 0 && customers == 0 then
        0
      else
        (toFloat customersScore / toFloat total) * 100
    , if total == 0 && fraudsters == 0 && customers == 0 then
        0
      else
        (toFloat superbadGuyScore / toFloat total) * 100
    )



randomList : Int -> Int -> List Int
randomList seed count =
    Tuple.first <| Random.step (Random.list count (Random.int 0 1000)) (Random.initialSeed seed)



randomTick : Int -> Int
randomTick seed =
    Tuple.first <| Random.step (Random.int 1 12) (Random.initialSeed seed)


scoreToLevel : ( Int, Int, Int ) -> Level
scoreToLevel ( fraudsters, customers, superbadGuy ) =
    let
        translatedScore =
            superbadGuy * 250 + fraudsters * 50
    in
    if translatedScore < 350 then
        Level1
    else if translatedScore >= 350 && translatedScore < 1000 then
        Level2
    else if translatedScore >= 1000 && translatedScore < 2000 then
        Level3
    else
        Level4


translateScore : ( Int, Int, Int ) -> Int
translateScore ( fraudsters, customers, superbadGuy ) =
    superbadGuy * 250 + fraudsters * 50 - customers * 100


calculatePlayingTime : Maybe Time -> Maybe Time -> String
calculatePlayingTime lastTick startedTime =
    case ( lastTick, startedTime ) of
        ( Just lastTick_, Just startedTime_ ) ->
            if lastTick_ > startedTime_ then
                "Played for " ++ toString (Time.inSeconds (lastTick_ - startedTime_)) ++ " Seconds"
            else
                ""

        _ ->
            ""


makeGrid : Int -> Dict Int ContentType -> List (Html Msg)
makeGrid rows gridContents =
    List.range 0 ((rows * rows) - 1)
        |> List.map
            (\index ->
                let
                    logoItem =
                        if index == floor ((toFloat rows * toFloat rows) / 2) then
                            [ class "grid-logo" ]
                        else
                            []

                    contentClass =
                        case Dict.get index gridContents of
                            Just Client ->
                                [ class "client" ]

                            Just Fraudster ->
                                [ class "fraudster" ]

                            Just SuperFraudster ->
                                [ class "super-fraudster" ]

                            _ ->
                                []
                in
                div
                    ([ class "grid-item" ]
                        ++ [ class ("grid-item-" ++ toString index) ]
                        ++ logoItem
                        ++ contentClass
                        ++ [ onClick (ClickBox index) ]
                    )
                    []
            )


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

