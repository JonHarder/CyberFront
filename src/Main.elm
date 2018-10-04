module Main exposing (..)

import Browser exposing (Document, UrlRequest)
import Browser.Navigation exposing (Key)
import Config exposing (Config)
import Css exposing (..)
import Game exposing (Game, createGame, getGame, getGameId, showGame)
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (css)
import Html.Styled.Events exposing (onClick)
import Http
import Json.Decode as Decode exposing (Decoder, Value, decodeValue)
import Json.Decode.Pipeline exposing (optional, required)
import Player exposing (Player, createPlayer, getPlayer, yourTurn)
import Ports exposing (savePhaseData)
import Pusher exposing (joinGame, newTurn)
import Session exposing (Session, decodeSession, saveSession)
import States.Loading as Loading
import States.Lobby as Lobby
import States.PreLoading as PreLoading
import States.TakingTurn as TakingTurn
import States.WaitingForTurn as WaitingForTurn
import Turn exposing (Turn, finishTurn, startTurn, turnEvent)
import Types exposing (Uuid, uuidToString)
import Unit exposing (Unit, getUnits)
import Url exposing (Url)
import Url.Parser exposing ((</>), (<?>), Parser, map, parse, s, top)
import Url.Parser.Query as Query


type alias TurnData =
    { turn : Turn
    }


type alias Model =
    { message : String
    , config : Config
    , state : State
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



{-
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
-}


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


fakeConfig : Config
fakeConfig =
    { apiUrl = ""
    , svgPath = ""
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

                _ =
                    Debug.log "parsed url" message
            in
            ( { message = message
              , config = config
              , state = PreLoadingState
              }
            , cmd
            )

        Err _ ->
            ( { message = "Failed to parse init config"
              , config =
                    fakeConfig
              , state = ConfigErrorState
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

        ( LobbyState data, NewTurn turnData ) ->
            let
                _ =
                    Debug.log "new turn!" turnData
            in
            case turnData of
                Just playerNumber ->
                    let
                        myTurn =
                            yourTurn data.player playerNumber

                        _ =
                            Debug.log "turn data" turnData
                    in
                    ( { model
                        | message = "got new turn event, player number: " ++ String.fromInt playerNumber
                        , state =
                            WaitingForTurnState
                                { game = data.game
                                , player = data.player
                                , units = []
                                , playerNumber = playerNumber
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

                        _ =
                            Debug.log "turn data" turnData
                    in
                    ( { model
                        | message = "got new turn event, player number: " ++ String.fromInt playerNumber
                      }
                    , if myTurn then
                        startTurn model.config.apiUrl data.player TurnStarted

                      else
                        Cmd.none
                    )

                Nothing ->
                    ( { model | message = "failed to parse new turn data" }, Cmd.none )

        ( WaitingForTurnState data, TurnStarted response ) ->
            case response of
                Ok turn ->
                    ( { model | state = TakingTurnState data { turn = turn } }
                    , Cmd.none
                    )

                Err _ ->
                    ( model, Cmd.none )

        ( state, NewTurn turnData ) ->
            let
                _ =
                    Debug.log "new turn in state" state
            in
            ( model, Cmd.none )

        _ ->
            ( model, Cmd.none )


gameView : Model -> List (Html Msg)
gameView model =
    case model.state of
        PreLoadingState ->
            [ h1 [] [ text "pre loading" ]
            ]

        LoadingState data ->
            [ h1 [] [ text "loading..." ]
            , Loading.view model.config data LoadingMsg
            ]

        LobbyState data ->
            [ h1 [] [ text "in lobby" ]
            , h2 [] [ text model.message ]
            , Lobby.view model.config data LobbyMsg
            ]

        TakingTurnState _ _ ->
            [ h1 [] [ text "taking a turn" ]
            , h2 [] [ text model.message ]
            ]

        WaitingForTurnState data ->
            [ h1 [] [ text "waiting for your turn" ]
            , h2 [] [ text model.message ]
            , h2 [] [ text <| "Its player number " ++ String.fromInt data.playerNumber ++ "'s turn" ]
            , WaitingForTurn.view model.config data WaitingForTurnMsg
            ]

        ConfigErrorState ->
            [ h1 [] [ text "you dun fucked a-a-ron" ]
            ]


view : Model -> Document Msg
view model =
    { title = "CyberWars"
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
