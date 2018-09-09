port module Pusher exposing (..)

import Api exposing (Game, getGameId)


port gameStarted : (String -> msg) -> Sub msg


type alias GameId =
    String


port bindToGame : GameId -> Cmd msg
