module Main exposing (InGameData, InTurnData, LobbyData, LobbyWithPlayerData, Model(..), Msg(..), PreLobbyData, beginTurn, gameView, init, lobbyView, lobbyWithPlayerView, main, parseGameId, preLobbyView, subscriptions, takingTurnView, update, updateLobby, updateLobbyWithPlayer, updatePreLobby, updateTakingTurn, updateWaitingForTurn, view, waitingForTurnView)

import Browser exposing (Document, UrlRequest)
import Browser.Navigation exposing (Key)
import Css exposing (..)
import Game exposing (Game, createGame, getGame, getGameId, showGame)
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (css)
import Html.Styled.Events exposing (onClick)
import Http
import Player exposing (Player, PlayerNumber, createPlayer, yourTurn)
import Pusher exposing (joinGame, newTurn)
import Turn exposing (Turn, finishTurn, startTurn, turnEvent)
import Types exposing (Uuid, uuidToString)
import Unit exposing (Unit, getUnits)
import Url exposing (Url)
import Url.Parser exposing ((<?>), Parser, map, parse, s, top)
import Url.Parser.Query as Query


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
    , playerNumber : PlayerNumber
    , units : List Unit
    }


type alias InTurnData =
    { apiUrl : String
    , message : String
    , player : Player
    , game : Game
    , playerNumber : PlayerNumber
    , units : List Unit
    , turn : Turn
    }


type Model
    = PreLobby PreLobbyData
    | Lobby LobbyData
    | LobbyWithPlayer LobbyWithPlayerData
    | TakingTurn InTurnData
    | WaitingForTurn InGameData


type Msg
    = GotGame (Result Http.Error Game)
    | GotPlayer (Result Http.Error Player)
    | NewTurn (Maybe PlayerNumber)
    | TurnStarted (Result Http.Error Turn)
    | EndTurn
    | TurnFinished
    | GotUnits (Result Http.Error (List Unit))
    | UrlChanged Url
    | UrlRequested UrlRequest


type ParsedUuid
    = ParsedUuid (Maybe String)


parseGame : Parser (ParsedUuid -> a) a
parseGame =
    map ParsedUuid (s "index.html" <?> Query.string "gameId")


parseGameId : Url -> Maybe String
parseGameId url =
    let
        _ =
            Debug.log "given url" url
    in
    case parse parseGame url of
        Just (ParsedUuid mGame) ->
            let
                _ =
                    Debug.log "successful parse" mGame
            in
            mGame

        Nothing ->
            let
                _ =
                    Debug.log "unsucessful parse" "fuck"
            in
            Nothing


init : String -> Url -> Key -> ( Model, Cmd Msg )
init apiUrl url key =
    let
        initCommand =
            case parseGameId url of
                Just gameId ->
                    let
                        _ =
                            Debug.log "gameId" gameId
                    in
                    getGame apiUrl gameId GotGame

                Nothing ->
                    let
                        _ =
                            Debug.log "successful parse, but no game id found" "god dammit"
                    in
                    createGame apiUrl GotGame
    in
    ( PreLobby { apiUrl = apiUrl, message = "Pre lobby" }
    , initCommand
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
                    let
                        _ =
                            Debug.log "fuck" err
                    in
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


beginTurn : InGameData -> ( Model, Cmd Msg )
beginTurn data =
    case yourTurn data.player data.playerNumber of
        True ->
            ( WaitingForTurn { data | message = "Starting turn..." }
            , startTurn data.apiUrl data.player TurnStarted
            )

        False ->
            ( WaitingForTurn { data | message = "its not your turn, silly. Cant begin turn." }, Cmd.none )


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
                    let
                        inGameData : InGameData
                        inGameData =
                            { apiUrl = data.apiUrl
                            , message = "received new turn event"
                            , game = data.game
                            , player = data.player
                            , playerNumber = playerNumber
                            , units = []
                            }
                    in
                    beginTurn inGameData

                Nothing ->
                    ( LobbyWithPlayer { data | message = "failed to parse turn" }, Cmd.none )

        _ ->
            ( model, Cmd.none )


updateTakingTurn : Msg -> InTurnData -> ( Model, Cmd Msg )
updateTakingTurn msg data =
    case msg of
        EndTurn ->
            ( WaitingForTurn
                { apiUrl = data.apiUrl
                , message = "turn complete!"
                , game = data.game
                , player = data.player
                , playerNumber = data.playerNumber
                , units = data.units
                }
            , finishTurn data.apiUrl data.turn (\_ -> TurnFinished)
            )

        _ ->
            ( TakingTurn data, Cmd.none )


updateWaitingForTurn : Msg -> InGameData -> ( Model, Cmd Msg )
updateWaitingForTurn msg data =
    case msg of
        TurnStarted result ->
            case result of
                Ok turn ->
                    let
                        inTurnData =
                            { apiUrl = data.apiUrl
                            , message = "turn started. do your thing."
                            , game = data.game
                            , player = data.player
                            , playerNumber = data.playerNumber
                            , units = []
                            , turn = turn
                            }
                    in
                    ( TakingTurn inTurnData
                    , Cmd.none
                    )

                Err _ ->
                    ( WaitingForTurn
                        { data | message = "failed to parse turn start request" }
                    , Cmd.none
                    )

        NewTurn turnData ->
            case turnData of
                Just playerNumber ->
                    let
                        inGameData : InGameData
                        inGameData =
                            { apiUrl = data.apiUrl
                            , message = "received new turn event"
                            , game = data.game
                            , player = data.player
                            , playerNumber = playerNumber
                            , units = []
                            }
                    in
                    beginTurn inGameData

                Nothing ->
                    ( WaitingForTurn { data | message = "failed to parse turn" }, Cmd.none )

        _ ->
            ( WaitingForTurn data, Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case model of
        PreLobby data ->
            updatePreLobby msg data

        Lobby data ->
            updateLobby msg data

        LobbyWithPlayer data ->
            updateLobbyWithPlayer msg data

        TakingTurn data ->
            updateTakingTurn msg data

        WaitingForTurn data ->
            updateWaitingForTurn msg data


preLobbyView : PreLobbyData -> List (Html Msg)
preLobbyView data =
    [ h1 [] [ text data.message ] ]


lobbyView : LobbyData -> List (Html Msg)
lobbyView data =
    [ h1 [] [ text "Lobby" ]
    , h2 [] [ text data.message ]
    , h3 [] [ text <| "game: " ++ uuidToString (getGameId data.game) ]
    , div
        [ css [ margin (px 30) ]
        ]
        [ showGame data.game ]
    ]


lobbyWithPlayerView : LobbyWithPlayerData -> List (Html Msg)
lobbyWithPlayerView data =
    [ h1 [] [ text "Lobby...with a player" ]
    , h2 [] [ text data.message ]
    , h3 [] [ text <| "game: " ++ uuidToString (getGameId data.game) ]
    , div
        [ css [ margin (px 30) ]
        ]
        [ showGame data.game ]
    ]


waitingForTurnView : InGameData -> List (Html Msg)
waitingForTurnView data =
    [ h1 [] [ text "In Game!" ]
    , h2 [] [ text data.message ]
    , h2 []
        [ text "Not your turn" ]
    , div
        [ css [ margin (px 30) ]
        ]
        [ showGame data.game ]
    ]


takingTurnView : InTurnData -> List (Html Msg)
takingTurnView data =
    [ h1 [] [ text "Taking a turn" ]
    , h2 [] [ text data.message ]
    , div
        [ css [ margin (px 30) ]
        ]
        [ showGame data.game ]
    , button [ onClick EndTurn ] [ text "end turn" ]
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

        WaitingForTurn data ->
            waitingForTurnView data

        TakingTurn data ->
            takingTurnView data


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

        WaitingForTurn _ ->
            newTurn (turnEvent NewTurn)

        _ ->
            Sub.none


main : Program String Model Msg
main =
    Browser.application
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        , onUrlChange = UrlChanged
        , onUrlRequest = UrlRequested
        }
