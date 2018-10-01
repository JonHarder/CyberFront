module Phases.PreLobby exposing (Model, Msg, createOrJoinGame, initModel, update, view)

import Config exposing (Config)
import Game exposing (Game, createGame, getGame)
import Html.Styled exposing (Html, h1, text)
import Http
import Player exposing (createPlayer)
import Pusher exposing (joinGame)


type alias Model =
    { message : String
    , config : Config
    }


type Msg
    = GotGame (Result Http.Error Game)


initModel : Config -> Model
initModel config =
    { message = "Pre Lobby", config = config }


update : Msg -> Model -> ( Model, Cmd Msg )
update (GotGame result) model =
    case result of
        Err _ ->
            ( { model | message = "failed to create game" }, Cmd.none )

        Ok game ->
            ( { model | message = "got a game!" }
            , Cmd.batch
                [ joinGame game

                -- , createPlayer model.config.apiUrl game GotPlayer
                ]
            )


view : Model -> List (Html Msg)
view model =
    [ h1 [] [ text model.message ] ]


createOrJoinGame : String -> Maybe String -> Cmd Msg
createOrJoinGame apiUrl maybeGameId =
    case maybeGameId of
        Just gameId ->
            getGame apiUrl gameId GotGame

        Nothing ->
            createGame apiUrl GotGame
