module Api exposing (Game, Player, Uuid, getGameId, requestCreatePlayer, requestGame, requestMap, showGame, showPlayer)

import Http
import Json.Decode as Decode exposing (Decoder, string)
import Json.Decode.Pipeline exposing (hardcoded, required)
import Json.Encode as Encode exposing (Value)
import Map exposing (..)
import Unit exposing (..)



-- TODO: use elm/url Url builders


type Uuid
    = Uuid String


type alias GameInternals =
    { id : Uuid
    , currentPlayerNumber : Int
    }


type Game
    = Game GameInternals


type alias PlayerInternals =
    { id : Uuid
    , playerNumber : Int
    }


type Player
    = Player PlayerInternals


showPlayer : Player -> String
showPlayer (Player player) =
    let
        (Uuid id) =
            player.id
    in
    "Player: " ++ id ++ ", num: " ++ String.fromInt player.playerNumber


getGameId : Game -> String
getGameId (Game game) =
    let
        (Uuid id) =
            game.id
    in
    id


showGame : Game -> String
showGame (Game game) =
    let
        (Uuid id) =
            game.id
    in
    "Game " ++ id ++ ", current player num: " ++ String.fromInt game.currentPlayerNumber


url : String
url =
    "http://localhost:3000"


decodeUuid : Decoder Uuid
decodeUuid =
    Decode.map Uuid string


decodeGame : Decoder Game
decodeGame =
    Decode.succeed GameInternals
        |> required "id" decodeUuid
        |> required "currentPlayerNumber" Decode.int
        |> Decode.map Game


encodeGame : Game -> Value
encodeGame game =
    Encode.object [ ( "gameId", Encode.string (getGameId game) ) ]


decodePlayer : Decoder Player
decodePlayer =
    Decode.succeed PlayerInternals
        |> required "id" decodeUuid
        |> required "playerNumber" Decode.int
        |> Decode.map Player


decodeTile : Decoder Tile
decodeTile =
    Decode.succeed (Tile { terrain = Walkable })


decodeUnit : Decoder (Maybe Unit)
decodeUnit =
    Decode.succeed Nothing


decodeMap : Decoder Map
decodeMap =
    Decode.succeed MapInternals
        |> required "tiles" (Decode.list decodeTile)
        |> required "units" (Decode.list decodeUnit)
        |> Decode.map Map


requestGame : Http.Request Game
requestGame =
    let
        endpoint =
            url ++ "/game"
    in
    Http.post endpoint Http.emptyBody decodeGame


requestCreatePlayer : Game -> Http.Request Player
requestCreatePlayer game =
    let
        endpoint =
            url ++ "/player"

        payload =
            encodeGame game

        body =
            Http.jsonBody payload
    in
    Http.post endpoint body decodePlayer


requestMap : String -> Http.Request Map
requestMap mapId =
    let
        endpoint =
            url ++ "/map/" ++ mapId
    in
    Http.get endpoint decodeMap
