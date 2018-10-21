module Main exposing (..)

import Browser exposing (Document, UrlRequest)
import Browser.Navigation exposing (Key)
import Config exposing (Config)
import Css exposing (..)
import Game exposing (Game, createGame, currentPlayer, getGame, getGameId, showGame, updatePlayerNumber)
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (css, disabled)
import Html.Styled.Events exposing (onClick)
import Http
import Json.Decode as Decode exposing (Decoder, Value, decodeValue)
import Json.Decode.Pipeline exposing (optional, required)
import Player exposing (Player, createPlayer, getPlayer, getPlayerNumber, showPlayer, yourTurn)
import Ports exposing (resetGame)
import Pusher exposing (joinGame, newTurn)
import Session exposing (Session, decodeSession, saveSession)
import States.Loading as Loading
import States.Lobby as Lobby
import States.PreLoading as PreLoading
import States.TakingTurn as TakingTurn
import States.WaitingForTurn as WaitingForTurn
import Turn exposing (Turn, finishTurn, startTurn, turnEvent)
import Types exposing (Uuid, uuidToString)
import Unit exposing (Unit, detailedUnitView, getUnits, unitCoordinates, unitOwner, viewUnit)
import Url exposing (Url)
import Url.Parser exposing ((</>), (<?>), Parser, map, parse, s, top)
import Url.Parser.Query as Query
import Util exposing (pluralize)


type alias TurnData =
    { turn : Turn
    }


type alias Model =
    { message : String
    , config : Config
    , state : State
    , title : String
    }


type State
    = PreLoadingState
    | LoadingState Loading.Model
    | LobbyState Lobby.Model
    | TakingTurnState WaitingForTurn.Model TurnData
    | WaitingForTurnState WaitingForTurn.Model
    | ConfigErrorState


type Msg
    = PreLoadingMsg PreLoading.Msg
    | LoadingMsg Loading.Msg
    | LobbyMsg Lobby.Msg
    | TakingTurnMsg TakingTurn.Msg
    | WaitingForTurnMsg WaitingForTurn.Msg
    | UrlChanged Url
    | UrlRequested UrlRequest
    | NewTurn (Maybe Int)
    | TurnStarted (Result Http.Error Turn)
    | GotUnits (Result Http.Error (List Unit))
    | Selected Unit
    | TurnFinished
    | EndTurn
    | NoOp
    | NewGame


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


decodeConfig : Decoder Config
decodeConfig =
    Decode.succeed Config
        |> required "apiUrl" Decode.string
        |> required "svgPath" Decode.string
        |> required "state" (Decode.nullable decodeSession)


parseFlags : Value -> Result Decode.Error Config
parseFlags value =
    decodeValue decodeConfig value


fakeConfig : Config
fakeConfig =
    { apiUrl = ""
    , svgPath = ""
    , session = Nothing
    }

emptySessionConfig : Config -> Config
emptySessionConfig config =
    { apiUrl = config.apiUrl
    , svgPath = config.svgPath
    , session = Nothing
    }


init : Value -> Url -> Key -> ( Model, Cmd Msg )
init flags url key =
    case parseFlags flags of
        Ok config ->
            let
                urlGameId =
                    parseGameId url

                cmd =
                    Cmd.map PreLoadingMsg <| PreLoading.initCmd urlGameId config

                message =
                    case ( config.session, urlGameId ) of
                        ( _, Just gameId ) ->
                            "Joining existing game"

                        ( Just session, _ ) ->
                            "Resuming previous session"

                        _ ->
                            "Starting a new game"

                preLobbyConfig =
                     case ( config.session, urlGameId ) of
                         ( Just session, Just gameId ) ->
                             if gameId /= session.gameId then emptySessionConfig config else config

                         _ ->
                             config

                _ =
                    Debug.log "parsed url" message
            in
            ( { message = message
              , config = preLobbyConfig
              , state = PreLoadingState
              , title = "Loading"
              }
            , cmd
            )

        Err _ ->
            ( { message = "Failed to parse init config"
              , config =
                    fakeConfig
              , state = ConfigErrorState
              , title = "..."
              }
            , Cmd.none
            )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( model.state, msg ) of
        ( PreLoadingState, PreLoadingMsg subMsg ) ->
            let
                onFailToModel message =
                    { model | message = message }

                onSuccessToModel message game =
                    { model | message = message, state = LoadingState { game = game } }

                toMsg response =
                    LoadingMsg <| Loading.GotPlayer response
            in
            PreLoading.update model.config subMsg onFailToModel onSuccessToModel toMsg

        ( LoadingState data, LoadingMsg subMsg ) ->
            let
                onFailToModel message =
                    { model | message = message }

                onSuccessToModel message game player =
                    { model | state = LobbyState { player = player, game = game } }
            in
            Loading.update subMsg data onFailToModel onSuccessToModel

        ( LobbyState _, NewGame ) ->
            ( model, resetGame () )

        ( LobbyState data, NewTurn turnData ) ->
            case turnData of
                Just playerNumber ->
                    let
                        myTurn =
                            yourTurn data.player playerNumber
                    in
                    ( { model
                        | message = "got new turn event, player number: " ++ String.fromInt playerNumber
                        , title = "In Game"
                        , state =
                            WaitingForTurnState
                                { game = updatePlayerNumber data.game playerNumber
                                , player = data.player
                                , units = []
                                , selectedUnit = Nothing
                                }
                      }
                    , if myTurn then
                        startTurn model.config.apiUrl data.player TurnStarted

                      else
                        Cmd.none
                    )

                Nothing ->
                    ( { model | message = "failed to parse new turn data" }, Cmd.none )

        ( WaitingForTurnState data, NewTurn turnData ) ->
            case turnData of
                Just playerNumber ->
                    let
                        myTurn =
                            yourTurn data.player playerNumber
                    in
                    ( { model
                        | message = "got new turn event, player number: " ++ String.fromInt playerNumber
                        , title = "In Game"
                      }
                    , if myTurn then
                        startTurn model.config.apiUrl data.player TurnStarted

                      else
                        Cmd.none
                    )

                Nothing ->
                    ( { model | message = "failed to parse new turn data" }, Cmd.none )

        ( LobbyState data, TurnStarted response ) ->
            let
                _ =
                    Debug.log "got a turn started event in lobby state somehow" ""
            in
            ( model, Cmd.none )

        ( WaitingForTurnState data, TurnStarted response ) ->
            case response of
                Ok turn ->
                    ( { model | state = TakingTurnState data { turn = turn } }
                    , getUnits model.config.apiUrl data.game GotUnits
                    )

                Err _ ->
                    ( model, Cmd.none )

        ( TakingTurnState data turn, GotUnits response ) ->
            case response of
                Ok units ->
                    ( { model | state = TakingTurnState { data | units = units } turn }
                    , Cmd.none
                    )

                Err e ->
                    let
                        _ =
                            Debug.log "units" e
                    in
                    ( { model | message = "failed to get units" }
                    , Cmd.none
                    )

        ( TakingTurnState data turn, EndTurn ) ->
            ( { model | state = WaitingForTurnState data, message = "turn finished" }
            , finishTurn model.config.apiUrl turn.turn (\_ -> TurnFinished)
            )

        ( TakingTurnState data turn, Selected unit ) ->
            ( { model | state = TakingTurnState (WaitingForTurn.selectUnit data unit) turn }
            , Cmd.none
            )

        _ ->
            ( model, Cmd.none )


unplacedUnits : List Unit -> Player -> List Unit
unplacedUnits units player =
    let
        ownedUnit unit =
            case unitOwner unit of
                Just owner ->
                    owner == getPlayerNumber player

                Nothing ->
                    False
    in
    List.filter ownedUnit units


gameView : Model -> List (Html Msg)
gameView model =
    case model.state of
        PreLoadingState ->
            [ h1 [] [ text "pre loading" ]
            , h2 [] [ text model.message ]
            ]

        LoadingState data ->
            [ h1 [] [ text "loading..." ]
            , h2 [] [ text model.message ]
            , Loading.view model.config data LoadingMsg
            ]

        LobbyState data ->
            [ h1 [] [ text "in lobby" ]
            , h2 [] [ text model.message ]
            , showPlayer data.player
            , Lobby.view model.config data LobbyMsg
            , button [ onClick NewGame ] [ text "new game" ]
            ]

        TakingTurnState data _ ->
            let
                unplaced =
                    unplacedUnits data.units data.player
            in
            [ h1 [] [ text "taking a turn" ]
            , h2 [] [ text model.message ]
            , WaitingForTurn.view model.config data (\_ -> NoOp)
            , h2 [ css [ marginRight (px 30) ] ] (List.map (viewUnit Selected) unplaced)
            , span []
                [ case data.selectedUnit of
                    Just unit ->
                        detailedUnitView (\_ -> NoOp) unit

                    Nothing ->
                        text "nothing selected"
                ]
            , button [ onClick EndTurn, disabled (List.length unplaced > 0) ] [ text "End Turn" ]
            ]

        WaitingForTurnState data ->
            [ h1 [] [ text "waiting for your turn" ]
            , h2 [] [ text model.message ]
            , WaitingForTurn.view model.config data WaitingForTurnMsg
            ]

        ConfigErrorState ->
            [ h1 [] [ text "you dun fucked a-a-ron" ]
            , h2 [] [ text model.message ]
            ]


view : Model -> Document Msg
view model =
    { title = "CyberWars | " ++ model.title
    , body = List.map toUnstyled <| gameView model
    }


subscriptions : Model -> Sub Msg
subscriptions model =
    newTurn (turnEvent NewTurn)



{-
   case model.state of
       LobbyState _ ->
           newTurn (turnEvent NewTurn)

       WaitingForTurnState _ ->
           newTurn (turnEvent NewTurn)

       _ ->
           Sub.none
-}
{-
   case model of
       LobbyWithPlayer _ ->
           newTurn (turnEvent NewTurn)

       WaitingForTurn _ ->
           newTurn (turnEvent NewTurn)

       _ ->
           Sub.none
-}


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
