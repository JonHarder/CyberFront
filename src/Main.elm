module Main exposing (..)

import Api exposing (requestCreatePlayer, requestGame)
import Browser exposing (Document)
import Css exposing (..)
import Game exposing (Game, showGame)
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (css)
import Html.Styled.Events exposing (onClick)
import Http
import Json.Encode exposing (Value)
import Map exposing (..)
import Player exposing (Player, showPlayer)
import Pusher exposing (Turn, joinGame, makeTurn, newTurn, turnNumber)
import Types exposing (Coord, Dimensions)


type alias PreLobbyData =
    { apiUrl : String, message : String }


type alias LobbyData =
    { message : String
    , game : Game
    , apiUrl : String
    }


type alias LobbyWithPlayerData =
    { apiUrl : String
    , game : Game
    , player : Player
    , message : String
    }


type alias InGameData =
    { message : String
    , game : Game
    , player : Player
    , apiUrl : String
    , turn : Turn
    }


type Model
    = PreLobby PreLobbyData
    | Lobby LobbyData
    | LobbyWithPlayer LobbyWithPlayerData
    | InGame InGameData


type Msg
    = GotGame (Result Http.Error Game)
    | GotPlayer (Result Http.Error Player)
    | NewTurn (Maybe Turn)


createGame : String -> Cmd Msg
createGame apiUrl =
    Http.send GotGame (requestGame apiUrl)


getPlayer : String -> Game -> Cmd Msg
getPlayer apiUrl game =
    Http.send GotPlayer (requestCreatePlayer apiUrl game)


init : String -> ( Model, Cmd Msg )
init apiUrl =
    ( PreLobby { apiUrl = apiUrl, message = "Pre lobby" }
    , createGame apiUrl
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
                    , Cmd.batch [ joinGame game, getPlayer data.apiUrl game ]
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
                        , message = "Created player!"
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
                Just turn ->
                    ( InGame
                        { message = "Game started! Turn: " ++ String.fromInt (turnNumber turn)
                        , player = data.player
                        , game = data.game
                        , apiUrl = data.apiUrl
                        , turn = turn
                        }
                    , Cmd.none
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
                Just turn ->
                    ( InGame
                        { data | message = "Turn: " ++ String.fromInt (turnNumber turn) }
                    , Cmd.none
                    )

                Nothing ->
                    ( model, Cmd.none )

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
    , div [ css [ margin (px 30) ] ]
        [ showPlayer data.player
        ]
    ]


inGameView : InGameData -> List (Html Msg)
inGameView data =
    [ h1 [] [ text "In Game!" ]
    , h2 [] [ text data.message ]
    , div
        [ css [ margin (px 30) ]
        ]
        [ showGame data.game
        , showPlayer data.player
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
            newTurn (makeTurn NewTurn)

        InGame _ ->
            newTurn (makeTurn NewTurn)

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
