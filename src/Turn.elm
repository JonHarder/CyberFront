module Turn exposing (Turn, startTurn, turnEvent)

import Http
import Json.Decode as Decode exposing (Decoder, Value, decodeValue, int)
import Json.Decode.Pipeline exposing (required)
import Player exposing (Player, encodePlayer)
import Types exposing (Uuid, decodeUuid)


type TurnStatus
    = InProgress
    | Complete


type alias TurnInternals =
    { id : Uuid
    , status : TurnStatus
    }


type Turn
    = Turn TurnInternals


type alias TurnEvent =
    { playerNumber : Int }


decodeTurnEvent : Decoder TurnEvent
decodeTurnEvent =
    Decode.succeed TurnEvent
        |> required "playerNumber" int


turnEvent : (Maybe Int -> msg) -> Value -> msg
turnEvent makeMsg value =
    case decodeValue decodeTurnEvent value of
        Ok turnData ->
            makeMsg <| Just turnData.playerNumber

        _ ->
            makeMsg Nothing


startTurn : String -> Player -> (Result Http.Error Turn -> msg) -> Cmd msg
startTurn apiUrl player turnMsg =
    let
        endpoint =
            apiUrl ++ "/turns"

        body =
            Http.jsonBody <| encodePlayer player

        startTurnRequest =
            Http.post endpoint body decodeTurn
    in
    Http.send turnMsg startTurnRequest


decodeStatus : Decoder TurnStatus
decodeStatus =
    Decode.string
        |> Decode.andThen statusHelper


statusHelper : String -> Decoder TurnStatus
statusHelper statusString =
    case statusString of
        "in-progress" ->
            Decode.succeed InProgress

        "complete" ->
            Decode.succeed Complete

        _ ->
            Decode.fail <|
                "Trying to decode a turn status, but status: "
                    ++ statusString
                    ++ " is not supported."


decodeTurn : Decoder Turn
decodeTurn =
    Decode.succeed TurnInternals
        |> required "id" decodeUuid
        |> required "status" decodeStatus
        |> Decode.map Turn
