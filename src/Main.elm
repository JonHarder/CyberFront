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
    { message : String, gameLoading : Maybe Game }


type alias LobbyData =
    { message : String
    , game : Game
    , player : Player
    }


type alias InGameData =
    { message : String
    , game : Game
    , player : Player
    , map : Map
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


createGame : Cmd Msg
createGame =
    Http.send GotGame requestGame


getPlayer : Game -> Cmd Msg
getPlayer game =
    Http.send GotPlayer (requestCreatePlayer game)


getMap : String -> Cmd Msg
getMap mapId =
    Http.send GotMap (requestMap mapId)


init : flags -> ( Model, Cmd Msg )
init _ =
    ( PreLobby { message = "Pre lobby", gameLoading = Nothing }
    , createGame
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
                    , Cmd.batch [ joinGame game, getPlayer game ]
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
            ( model, getMap mapId )

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


main : Program Value Model Msg
main =
    Browser.document
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }
