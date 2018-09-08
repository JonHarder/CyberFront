module Api exposing (Game, Uuid, requestGame, showGame)

import Http
import Json.Decode as Decode exposing (Decoder, string)


type Uuid
    = Uuid String


type Game
    = Game
        { id : Uuid
        }


getGameId : Game -> Uuid
getGameId (Game game) =
    game.id


showGame : Game -> String
showGame (Game game) =
    let
        (Uuid id) =
            game.id
    in
    "Game " ++ id


url : String
url =
    "http://localhost:3000"


decodeUuid : Decoder Uuid
decodeUuid =
    Decode.map Uuid string


decodeGame : Decoder Game
decodeGame =
    Decode.map (\id -> Game { id = id }) (Decode.field "id" decodeUuid)


requestGame : Http.Request Game
requestGame =
    let
        endpoint =
            url ++ "/" ++ "game"
    in
    Http.post endpoint Http.emptyBody decodeGame
