module Main exposing (..)

import Api exposing (Game, Player, getGameId, requestCreatePlayer, requestGame, requestMap, showGame, showPlayer)
import Browser exposing (Document)
import Css exposing (..)
import Html
import Html.Styled exposing (..)
import Html.Styled.Events exposing (onClick)
import Http
import Json.Encode exposing (Value)
import Map exposing (..)
import Pusher exposing (gameStarted, joinGame)


type alias PreLobbyData =
    { apiUrl : String, message : String, gameLoading : Maybe Game }


type alias LobbyData =
    { message : String
    , game : Game
    , player : Player
    , apiUrl : String
    }


type alias InGameData =
    { message : String
    , game : Game
    , player : Player
    , map : Map
    , apiUrl : String
    }


type Model
    = PreLobby PreLobbyData
    | Lobby LobbyData
    | InGame InGameData


type Msg
    = GotGame (Result Http.Error Game)
    | GotPlayer (Result Http.Error Player)
    | GotMapId String
    | GotMap (Result Http.Error Map)


createGame : String -> Cmd Msg
createGame apiUrl =
    Http.send GotGame (requestGame apiUrl)


getPlayer : String -> Game -> Cmd Msg
getPlayer apiUrl game =
    Http.send GotPlayer (requestCreatePlayer apiUrl game)


getMap : String -> String -> Cmd Msg
getMap apiUrl mapId =
    Http.send GotMap (requestMap apiUrl mapId)


init : String -> ( Model, Cmd Msg )
init apiUrl =
    ( PreLobby { apiUrl = apiUrl, message = "Pre lobby", gameLoading = Nothing }
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
                    ( PreLobby
                        { data
                            | message = "created game, creating player"
                            , gameLoading = Just game
                        }
                    , Cmd.batch [ joinGame game, getPlayer data.apiUrl game ]
                    )

        GotPlayer result ->
            case result of
                Err err ->
                    ( PreLobby { data | message = "failed to join game" }, Cmd.none )

                Ok player ->
                    case data.gameLoading of
                        Just game ->
                            ( Lobby
                                { message = "In Lobby. Waiting for more players"
                                , game = game
                                , player = player
                                , apiUrl = data.apiUrl
                                }
                            , Cmd.none
                            )

                        Nothing ->
                            ( PreLobby { data | message = "no reference to game :(" }, Cmd.none )

        _ ->
            ( model, Cmd.none )


updateLobby : Msg -> LobbyData -> ( Model, Cmd Msg )
updateLobby msg data =
    let
        model =
            Lobby data
    in
    case msg of
        GotMapId mapId ->
            ( model, getMap data.apiUrl mapId )

        GotMap result ->
            case result of
                Err e ->
                    let
                        _ =
                            Debug.log "failed to get map" e
                    in
                    ( model, Cmd.none )

                Ok map ->
                    ( InGame
                        { message = "Game Started!"
                        , game = data.game
                        , player = data.player
                        , map = map
                        , apiUrl = data.apiUrl
                        }
                    , Cmd.none
                    )

        _ ->
            ( model, Cmd.none )


updateInGame : Msg -> InGameData -> ( Model, Cmd Msg )
updateInGame msg data =
    ( InGame data, Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case model of
        PreLobby data ->
            updatePreLobby msg data

        Lobby data ->
            updateLobby msg data

        InGame data ->
            updateInGame msg data


preLobbyView : PreLobbyData -> List (Html Msg)
preLobbyView data =
    [ h1 [] [ text data.message ] ]


lobbyView : LobbyData -> List (Html Msg)
lobbyView data =
    [ h1 [] [ text "Lobby" ]
    , h2 [] [ text data.message ]
    , h3 [] [ text (showGame data.game) ]
    , h3 [] [ text (showPlayer data.player) ]
    ]


viewMap : Map -> Html Msg
viewMap map =
    h3 []
        [ text <| "map width: " ++ String.fromInt (getMapWidth map)
        ]


inGameView : InGameData -> List (Html Msg)
inGameView data =
    [ h1 [] [ text "In Game!" ]
    , viewMap data.map
    ]


gameView : Model -> List (Html Msg)
gameView model =
    case model of
        PreLobby data ->
            preLobbyView data

        Lobby data ->
            lobbyView data

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
        PreLobby _ ->
            gameStarted GotMapId

        Lobby _ ->
            gameStarted GotMapId

        _ ->
            Sub.none


type alias Config =
    { api_url : String
    }


main : Program String Model Msg
main =
    Browser.document
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }
