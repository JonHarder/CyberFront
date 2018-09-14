module Turn exposing (Turn, decodeTurn)

import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline exposing (required)
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
