port module Pusher exposing (gameStarted, joinGame)

import Api exposing (Game, getGameId)


port gameStarted : (String -> msg) -> Sub msg


joinGame : Game -> Cmd msg
joinGame game =
    bindToGame <| getGameId game


port bindToGame : String -> Cmd msg
