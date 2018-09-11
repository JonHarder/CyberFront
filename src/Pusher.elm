port module Pusher exposing (joinGame, newTurn)

import Api exposing (Game, getGameId)


port newTurn : (Int -> msg) -> Sub msg


joinGame : Game -> Cmd msg
joinGame game =
    bindToGame <| getGameId game


port bindToGame : String -> Cmd msg
