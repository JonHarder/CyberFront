module Main exposing (InGameData, LobbyData, LobbyWithPlayerData, Model(..), Msg(..), PreLobbyData, gameView, inGameView, init, lobbyView, lobbyWithPlayerView, main, preLobbyView, subscriptions, update, updateInGame, updateLobby, updateLobbyWithPlayer, updatePreLobby, view)

import Browser exposing (Document)
import Css exposing (..)
import Game exposing (Game, createGame, showGame)
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (css)
import Html.Styled.Events exposing (onClick)
import Http
import Player exposing (Player, createPlayer, yourTurn)
import Pusher exposing (joinGame, newTurn)
import Turn exposing (turnEvent)
import Unit exposing (Unit, getUnits)


type alias PreLobbyData =
    { apiUrl : String
    , message : String
    }


type alias LobbyData =
    { apiUrl : String
    , message : String
    , game : Game
    }


type alias LobbyWithPlayerData =
    { apiUrl : String
    , message : String
    , game : Game
    , player : Player
    }


type alias InGameData =
    { apiUrl : String
    , message : String
    , game : Game
    , player : Player
    , playerNumber : Int
    , units : List Unit
    }


type Model
    = PreLobby PreLobbyData
    | Lobby LobbyData
    | LobbyWithPlayer LobbyWithPlayerData
    | InGame InGameData


type Msg
    = GotGame (Result Http.Error Game)
    | GotPlayer (Result Http.Error Player)
    | NewTurn (Maybe Int)
    | GotUnits (Result Http.Error (List Unit))


init : String -> ( Model, Cmd Msg )
init apiUrl =
    ( PreLobby { apiUrl = apiUrl, message = "Pre lobby" }
    , createGame apiUrl GotGame
    )


updatePreLobby : Msg -> PreLobbyData -> ( Model, Cmd Msg )
updatePreLobby msg data =
    let
        model =
            PreLobby data
    in
    case msg of
        GotGame result ->
            case result of
                Err err ->
                    ( PreLobby { data | message = "failed to create game" }, Cmd.none )

                Ok game ->
                    ( Lobby
                        { message = "created game, joining lobby..."
                        , game = game
                        , apiUrl = data.apiUrl
                        }
                    , Cmd.batch [ joinGame game, createPlayer data.apiUrl game GotPlayer ]
                    )

        _ ->
            ( model, Cmd.none )


updateLobby : Msg -> LobbyData -> ( Model, Cmd Msg )
updateLobby msg data =
    let
        model =
            Lobby data
    in
    case msg of
        GotPlayer result ->
            case result of
                Err _ ->
                    ( Lobby
                        { data | message = "failed to create a player" }
                    , Cmd.none
                    )

                Ok player ->
                    ( LobbyWithPlayer
                        { apiUrl = data.apiUrl
                        , game = data.game
                        , player = player
                        , message = "Lobby joined.  Waiting for other players."
                        }
                    , Cmd.none
                    )

        _ ->
            ( model, Cmd.none )


updateLobbyWithPlayer : Msg -> LobbyWithPlayerData -> ( Model, Cmd Msg )
updateLobbyWithPlayer msg data =
    let
        model =
            LobbyWithPlayer data
    in
    case msg of
        NewTurn turnData ->
            case turnData of
                Just playerNumber ->
                    ( InGame
                        { message = "Game started! It's Player " ++ String.fromInt playerNumber ++ "s turn"
                        , player = data.player
                        , game = data.game
                        , apiUrl = data.apiUrl
                        , playerNumber = playerNumber
                        , units = []
                        }
                    , getUnits data.apiUrl data.game GotUnits
                    )

                Nothing ->
                    ( LobbyWithPlayer { data | message = "failed to parse turn" }, Cmd.none )

        _ ->
            ( model, Cmd.none )


updateInGame : Msg -> InGameData -> ( Model, Cmd Msg )
updateInGame msg data =
    let
        model =
            InGame data
    in
    case msg of
        NewTurn turnData ->
            case turnData of
                Just playerNumber ->
                    ( InGame
                        { data
                            | message = "Player " ++ String.fromInt playerNumber ++ "s turn"
                            , playerNumber = playerNumber
                        }
                    , Cmd.none
                    )

                Nothing ->
                    ( model, Cmd.none )

        GotUnits result ->
            case result of
                Err e ->
                    let
                        _ =
                            Debug.log "failed to get units" (Debug.toString e)
                    in
                    ( InGame { data | message = "Failed to get units" }, Cmd.none )

                Ok units ->
                    ( InGame
                        { data
                            | message = "Got units"
                            , units = units
                        }
                    , Cmd.none
                    )

        _ ->
            ( model, Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case model of
        PreLobby data ->
            updatePreLobby msg data

        Lobby data ->
            updateLobby msg data

        LobbyWithPlayer data ->
            updateLobbyWithPlayer msg data

        InGame data ->
            updateInGame msg data


preLobbyView : PreLobbyData -> List (Html Msg)
preLobbyView data =
    [ h1 [] [ text data.message ] ]


lobbyView : LobbyData -> List (Html Msg)
lobbyView data =
    [ h1 [] [ text "Lobby" ]
    , h2 [] [ text data.message ]
    , div
        [ css [ margin (px 30) ]
        ]
        [ showGame data.game ]
    ]


lobbyWithPlayerView : LobbyWithPlayerData -> List (Html Msg)
lobbyWithPlayerView data =
    [ h1 [] [ text "Lobby...with a player" ]
    , h2 [] [ text data.message ]
    , div
        [ css [ margin (px 30) ]
        ]
        [ showGame data.game ]
    ]


inGameView : InGameData -> List (Html Msg)
inGameView data =
    [ h1 [] [ text "In Game!" ]
    , h2 [] [ text data.message ]
    , h2 []
        [ text <|
            if yourTurn data.player data.playerNumber then
                "Your Turn"

            else
                "Not your turn"
        ]
    , div
        [ css
            [ property "display" "grid"
            , property "grid-template-columns" "2fr 1fr"
            , property "grid-template-rows" "2"
            ]
        ]
        [ div
            [ css
                [ property "grid-column-start" "1"
                , property "grid-column-end" "2"
                ]
            ]
            [ showGame data.game ]
        , div
            [ css
                [ property "grid-column-start" "2"
                , property "grid-column-end" "3"
                , backgroundColor (hex "555555")
                ]
            ]
            [ text "hello" ]
        ]
    ]


gameView : Model -> List (Html Msg)
gameView model =
    case model of
        PreLobby data ->
            preLobbyView data

        Lobby data ->
            lobbyView data

        LobbyWithPlayer data ->
            lobbyWithPlayerView data

        InGame data ->
            inGameView data


view : Model -> Document Msg
view model =
    { title = "CyberWars"
    , body = List.map toUnstyled <| gameView model
    }


subscriptions : Model -> Sub Msg
subscriptions model =
    case model of
        LobbyWithPlayer _ ->
            newTurn (turnEvent NewTurn)

        InGame _ ->
            newTurn (turnEvent NewTurn)

        _ ->
            Sub.none


main : Program String Model Msg
main =
    Browser.document
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }
