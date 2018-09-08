module Main exposing (..)

import Api exposing (Game, Player, requestCreatePlayer, requestGame, requestMap, showGame, showPlayer)
import Browser exposing (Document)
import Css exposing (..)
import Html
import Html.Styled exposing (..)
import Html.Styled.Events exposing (onClick)
import Http
import Json.Encode exposing (Value)
import Map exposing (..)
import Pusher exposing (gameStarted)


type alias Model =
    { game : Maybe Game
    , player : Maybe Player
    , message : String
    , map : Maybe Map
    }


type Msg
    = GotGame (Result Http.Error Game)
    | GotPlayer (Result Http.Error Player)
    | GotMapId String
    | GotMap (Result Http.Error Map)


createGame : Cmd Msg
createGame =
    Http.send GotGame requestGame


joinGame : Game -> Cmd Msg
joinGame game =
    Http.send GotPlayer (requestCreatePlayer game)


getMap : String -> Cmd Msg
getMap mapId =
    Http.send GotMap (requestMap mapId)


init : flags -> ( Model, Cmd Msg )
init _ =
    ( { game = Nothing
      , player = Nothing
      , message = "Initialized"
      , map = Nothing
      }
    , createGame
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotGame result ->
            case result of
                Err err ->
                    let
                        _ =
                            Debug.log "failed to create game" err
                    in
                    ( { model | message = "game creation failed" }, Cmd.none )

                Ok game ->
                    ( { model | message = "Game Created", game = Just game }, joinGame game )

        GotPlayer result ->
            case result of
                Err err ->
                    let
                        _ =
                            Debug.log "failed to join game" err
                    in
                    ( { model | message = "failed to join game" }, Cmd.none )

                Ok player ->
                    ( { model | message = "Joined game", player = Just player }, Cmd.none )

        GotMapId mapId ->
            let
                _ =
                    Debug.log "got map id" mapId
            in
            ( model, getMap mapId )

        GotMap result ->
            case result of
                Err _ ->
                    ( model, Cmd.none )

                Ok map ->
                    ( { model | map = Just map }, Cmd.none )


gameView : Model -> List (Html Msg)
gameView model =
    let
        gameString =
            case model.game of
                Just g ->
                    showGame g

                Nothing ->
                    "no game"

        playerString =
            case model.player of
                Just p ->
                    showPlayer p

                Nothing ->
                    "no player"
    in
    [ h1 [] [ text ("Message: " ++ model.message) ]
    , h3 [] [ text gameString ]
    , h4 [] [ text playerString ]
    ]


view : Model -> Document Msg
view model =
    { title = "CyberWars"
    , body = List.map toUnstyled <| gameView model
    }


subscriptions : Model -> Sub Msg
subscriptions model =
    gameStarted GotMapId


main : Program Value Model Msg
main =
    Browser.document
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }
