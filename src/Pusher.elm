port module Pusher exposing (joinGame, newTurn)

import Game exposing (Game, getGameId)
import Json.Decode as Decode exposing (Value)
import Types exposing (uuidToString)


port newTurn : (Value -> msg) -> Sub msg


joinGame : Game -> Cmd msg
joinGame game =
    let
        _ =
            Debug.log "binding to game" (getGameId game)
    in
    bindToGame <| uuidToString <| getGameId game


port bindToGame : String -> Cmd msg
