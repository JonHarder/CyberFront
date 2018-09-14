port module Pusher exposing (joinGame, newTurn)

import Game exposing (Game, getGameId)
import Types exposing (uuidToString)


port newTurn : (Int -> msg) -> Sub msg


joinGame : Game -> Cmd msg
joinGame game =
    bindToGame <| uuidToString <| getGameId game


port bindToGame : String -> Cmd msg
