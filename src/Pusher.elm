port module Pusher exposing (Turn, joinGame, makeTurn, newTurn, turnNumber)

import Game exposing (Game, getGameId)
import Json.Decode as Decode exposing (Decoder, Value, string)
import Json.Decode.Pipeline exposing (optional, required)
import Types exposing (uuidToString)


type Turn
    = Turn TurnData


type alias TurnData =
    { playerNumber : Int }


turnNumber : Turn -> Int
turnNumber (Turn data) =
    data.playerNumber


decodeTurn : Decoder Turn
decodeTurn =
    Decode.succeed TurnData
        |> required "playerNumber" Decode.int
        |> Decode.map Turn


makeTurn : (Maybe Turn -> msg) -> Value -> msg
makeTurn mkMsg value =
    case Decode.decodeValue decodeTurn value of
        Err _ ->
            mkMsg Nothing

        Ok turnData ->
            mkMsg <| Just turnData


port newTurn : (Value -> msg) -> Sub msg


joinGame : Game -> Cmd msg
joinGame game =
    bindToGame <| uuidToString <| getGameId game


port bindToGame : String -> Cmd msg
