module Session exposing (Session, decodeSession, saveSession)

import Game exposing (Game, getGameId)
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline exposing (required)
import Json.Encode as Encode
import Player exposing (Player, getPlayerId)
import Ports exposing (savePhaseData)
import Types exposing (uuidToString)


decodeSession : Decoder Session
decodeSession =
    Decode.succeed Session
        |> required "gameId" Decode.string
        |> required "playerId" Decode.string


encodeSession : { r | player : Player, game : Game } -> Encode.Value
encodeSession data =
    Encode.object
        [ ( "gameId", Encode.string (uuidToString <| getGameId data.game) )
        , ( "playerId", Encode.string (uuidToString <| getPlayerId data.player) )
        ]


saveSession : { r | player : Player, game : Game } -> Cmd msg
saveSession data =
    savePhaseData <| encodeSession data


type alias Session =
    { gameId : String
    , playerId : String
    }
