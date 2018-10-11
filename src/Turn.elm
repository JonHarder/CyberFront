module Turn exposing (Turn, finishTurn, startTurn, turnEvent)

import Http
import Json.Decode as Decode exposing (Decoder, Value, decodeValue, int)
import Json.Decode.Pipeline exposing (required)
import Json.Encode as Encode
import Player exposing (Player, decodePlayerNumber, encodePlayer)
import Types exposing (Uuid, decodeUuid, uuidToString)


type TurnStatus
    = InProgress
    | Complete


type alias TurnInternals =
    { id : Uuid
    , status : TurnStatus
    }


type Turn
    = Turn TurnInternals


encodeStatus : TurnStatus -> Value
encodeStatus status =
    let
        s =
            case status of
                InProgress ->
                    "in-progress"

                Complete ->
                    "turn-complete"
    in
    Encode.object [ ( "status", Encode.string s ) ]


getTurnId : Turn -> String
getTurnId (Turn data) =
    uuidToString data.id


type alias TurnEvent =
    { playerNumber : Int }


decodeTurnEvent : Decoder TurnEvent
decodeTurnEvent =
    Decode.succeed TurnEvent
        |> required "playerNumber" decodePlayerNumber


turnEvent : (Maybe Int -> msg) -> Value -> msg
turnEvent makeMsg value =
    let
        parsedEvent =
            case decodeValue decodeTurnEvent value of
                Ok data ->
                    Just data.playerNumber

                Err _ ->
                    Nothing
    in
    makeMsg parsedEvent


patch : String -> Http.Body -> Decoder val -> Http.Request val
patch url body decoder =
    Http.request
        { method = "PATCH"
        , headers = []
        , url = url
        , body = body
        , expect = Http.expectJson decoder
        , timeout = Nothing
        , withCredentials = False
        }


finishTurn : String -> Turn -> (Result Http.Error () -> msg) -> Cmd msg
finishTurn apiUrl (Turn data) turnMsg =
    let
        endpoint =
            apiUrl ++ "/turns/" ++ uuidToString data.id

        finishedTurn =
            encodeStatus Complete

        body =
            Http.jsonBody <| encodeStatus Complete

        request =
            patch endpoint body (Decode.succeed ())
    in
    Http.send turnMsg request


startTurn : String -> Player -> (Result Http.Error Turn -> msg) -> Cmd msg
startTurn apiUrl player turnMsg =
    let
        endpoint =
            apiUrl ++ "/turns"

        body =
            Http.jsonBody <| encodePlayer player

        request =
            Http.post endpoint body decodeTurn
    in
    Http.send turnMsg request


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
