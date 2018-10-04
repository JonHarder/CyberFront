module States.PreLoading exposing (..)

import Config exposing (Config)
import Game exposing (Game, createGame, getGame)
import Http
import Player exposing (Player, createPlayer, getPlayer)
import Pusher exposing (joinGame)
import Session exposing (Session)


type Msg
    = GotGame (Result Http.Error Game)


update : Config -> Msg -> (String -> model) -> (String -> Game -> model) -> (Result Http.Error Player -> msg) -> ( model, Cmd msg )
update config (GotGame response) toCurrentState toNextState toMsg =
    case response of
        Ok game ->
            ( toNextState "Created game" game
            , Cmd.batch
                [ case config.session of
                    Just session ->
                        getPlayer config.apiUrl session.playerId toMsg

                    Nothing ->
                        createPlayer config.apiUrl game toMsg
                , joinGame game
                ]
            )

        Err _ ->
            ( toCurrentState "failed to create game", Cmd.none )


initCmd : Maybe String -> Config -> Cmd Msg
initCmd urlGameId config =
    case ( urlGameId, config.session ) of
        ( Just gameId, _ ) ->
            getGame config.apiUrl gameId GotGame

        ( _, Just session ) ->
            getGame config.apiUrl session.gameId GotGame

        _ ->
            createGame config.apiUrl GotGame
