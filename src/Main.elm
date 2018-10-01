module Main exposing (..)

import Browser exposing (Document, UrlRequest)
import Browser.Navigation exposing (Key)
import Config exposing (Config)
import Css exposing (..)
import Game exposing (Game, createGame, getGame, getGameId, showGame)
import Html.Styled as Html exposing (..)
import Html.Styled.Attributes exposing (css)
import Html.Styled.Events exposing (onClick)
import Http
import Json.Decode as Decode exposing (Decoder, Value, decodeValue)
import Json.Decode.Pipeline exposing (optional, required)
import Phases.PreLobby as PreLobby
import Player exposing (Player, PlayerNumber, createPlayer, getPlayer, yourTurn)
import Ports exposing (savePhaseData)
import Pusher exposing (joinGame, newTurn)
import Session exposing (Session, decodeSession, saveSession)
import Turn exposing (Turn, finishTurn, startTurn, turnEvent)
import Types exposing (Uuid, uuidToString)
import Unit exposing (Unit, getUnits)
import Url exposing (Url)
import Url.Parser exposing ((</>), (<?>), Parser, map, parse, s, top)
import Url.Parser.Query as Query


type alias LobbyData =
    { message : String
    , game : Game
    , config : Config
    }


type alias LobbyWithPlayerData =
    { message : String
    , game : Game
    , player : Player
    , config : Config
    }


type alias InGameData =
    { message : String
    , game : Game
    , player : Player
    , playerNumber : PlayerNumber
    , units : List Unit
    , config : Config
    }


type alias InTurnData =
    { message : String
    , player : Player
    , game : Game
    , playerNumber : PlayerNumber
    , units : List Unit
    , turn : Turn
    , config : Config
    }


type Model
    = PreLobby PreLobby.Model
    | Lobby LobbyData
    | LobbyWithPlayer LobbyWithPlayerData
    | TakingTurn InTurnData
    | WaitingForTurn InGameData
    | ConfigError


type Msg
    = PreLobbyMsg PreLobby.Msg
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
    map ParsedUuid (top <?> Query.string "gameId")


parseGameId : Url -> Maybe String
parseGameId url =
    case parse parseGame url of
        Just (ParsedUuid mGame) ->
            mGame

        Nothing ->
            Nothing


configDecoder : Decoder Config
configDecoder =
    Decode.succeed Config
        |> required "apiUrl" Decode.string
        |> required "svgPath" Decode.string
        |> required "state" (Decode.nullable decodeSession)


parseFlags : Value -> Result Decode.Error Config
parseFlags value =
    decodeValue configDecoder value


getSuppliedGameId : Url -> Config -> Maybe String
getSuppliedGameId url config =
    case ( parseGameId url, config.session ) of
        ( Just gameId, _ ) ->
            Just gameId

        ( Nothing, Just session ) ->
            Just session.gameId

        ( _, _ ) ->
            Nothing


init : Value -> Url -> Key -> ( Model, Cmd Msg )
init flags url key =
    case parseFlags flags of
        Ok config ->
            let
                command =
                    PreLobby.createOrJoinGame config.apiUrl (getSuppliedGameId url config)
            in
            ( PreLobby (PreLobby.initModel config), Cmd.map PreLobbyMsg command )

        Err _ ->
            ( ConfigError, Cmd.none )


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
                    let
                        lobbyData =
                            { message = "Lobby joined.  Waiting for other players."
                            , game = data.game
                            , player = player
                            , config = data.config
                            }
                    in
                    ( LobbyWithPlayer lobbyData
                    , saveSession lobbyData
                    )

        _ ->
            ( model, Cmd.none )


beginTurn : InGameData -> ( Model, Cmd Msg )
beginTurn data =
    case yourTurn data.player data.playerNumber of
        True ->
            ( WaitingForTurn { data | message = "Starting turn..." }
            , startTurn data.config.apiUrl data.player TurnStarted
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
                            { message = "received new turn event"
                            , game = data.game
                            , player = data.player
                            , playerNumber = playerNumber
                            , units = []
                            , config = data.config
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
                { message = "turn complete!"
                , game = data.game
                , player = data.player
                , playerNumber = data.playerNumber
                , units = data.units
                , config = data.config
                }
            , finishTurn data.config.apiUrl data.turn (\_ -> TurnFinished)
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
                            { message = "turn started. do your thing."
                            , game = data.game
                            , player = data.player
                            , playerNumber = data.playerNumber
                            , units = []
                            , turn = turn
                            , config = data.config
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
                            { message = "received new turn event"
                            , game = data.game
                            , player = data.player
                            , playerNumber = playerNumber
                            , units = []
                            , config = data.config
                            }
                    in
                    beginTurn inGameData

                Nothing ->
                    ( WaitingForTurn { data | message = "failed to parse turn" }, Cmd.none )

        _ ->
            ( WaitingForTurn data, Cmd.none )


updateWith : (subModel -> Model) -> (subMsg -> Msg) -> Model -> ( subModel, Cmd subMsg ) -> ( Model, Cmd Msg )
updateWith toModel toMsg model ( subModel, subCmd ) =
    ( toModel subModel
    , Cmd.map toMsg subCmd
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( msg, model ) of
        ( PreLobbyMsg subMsg, PreLobby preLobbyModel ) ->
            PreLobby.update subMsg preLobbyModel
                |> updateWith PreLobby PreLobbyMsg model

        ( _, Lobby data ) ->
            updateLobby msg data

        ( _, LobbyWithPlayer data ) ->
            updateLobbyWithPlayer msg data

        ( _, TakingTurn data ) ->
            updateTakingTurn msg data

        ( _, WaitingForTurn data ) ->
            updateWaitingForTurn msg data

        ( _, ConfigError ) ->
            ( model, Cmd.none )

        ( _, _ ) ->
            ( model, Cmd.none )


lobbyView : LobbyData -> List (Html Msg)
lobbyView data =
    [ h1 [] [ text "Lobby" ]
    , h2 [] [ text data.message ]
    , h3 [] [ text <| "game: " ++ uuidToString (getGameId data.game) ]
    , div
        [ css [ margin (px 30) ]
        ]
        [ showGame data.config.svgPath data.game ]
    ]


lobbyWithPlayerView : LobbyWithPlayerData -> List (Html Msg)
lobbyWithPlayerView data =
    [ h1 [] [ text "Lobby...with a player" ]
    , h2 [] [ text data.message ]
    , h3 [] [ text <| "game: " ++ uuidToString (getGameId data.game) ]
    , div
        [ css [ margin (px 30) ]
        ]
        [ showGame data.config.svgPath data.game ]
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
        [ showGame data.config.svgPath data.game ]
    ]


takingTurnView : InTurnData -> List (Html Msg)
takingTurnView data =
    [ h1 [] [ text "Taking a turn" ]
    , h2 [] [ text data.message ]
    , div
        [ css [ margin (px 30) ]
        ]
        [ showGame data.config.svgPath data.game ]
    , button [ onClick EndTurn ] [ text "end turn" ]
    ]


gameView : Model -> List (Html Msg)
gameView model =
    case model of
        PreLobby data ->
            List.map (Html.map PreLobbyMsg) (PreLobby.view data)

        Lobby data ->
            lobbyView data

        LobbyWithPlayer data ->
            lobbyWithPlayerView data

        WaitingForTurn data ->
            waitingForTurnView data

        TakingTurn data ->
            takingTurnView data

        ConfigError ->
            [ h1 [] [ text "You dun fucked up a-a-ron" ] ]


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


main : Program Value Model Msg
main =
    Browser.application
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        , onUrlChange = UrlChanged
        , onUrlRequest = UrlRequested
        }
