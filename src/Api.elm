module Api exposing (getUnits)

import Game exposing (Game, encodeGame, getGameId)
import Http
import Json.Encode as Encode exposing (Value)
import Types exposing (uuidToString)
import Unit exposing (Unit, decodeUnits)



-- TODO: use elm/url Url builders


getUnits : String -> Game -> (Result Http.Error (List Unit) -> msg) -> Cmd msg
getUnits apiUrl game makeMsg =
    Http.send makeMsg (requestUnits apiUrl game)


requestUnits : String -> Game -> Http.Request (List Unit)
requestUnits apiUrl game =
    let
        endpoint =
            apiUrl ++ "/units/" ++ (uuidToString <| getGameId game)
    in
    Http.get endpoint decodeUnits
