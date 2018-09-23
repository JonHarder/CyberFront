module Api exposing (requestCreatePlayer)

import Game exposing (Game, encodeGame, getGameId)
import Http
import Json.Encode as Encode exposing (Value)
import Map exposing (Map, decodeMap)
import Player exposing (Player, decodePlayer, encodePlayer, getPlayerId, getPlayerNumber)
import Turn exposing (Turn, decodeTurn)
import Types exposing (Uuid, decodeCoord, decodeOwner, decodeUuid, uuidToString)
import Unit exposing (Unit, decodeUnits)



-- TODO: use elm/url Url builders


requestUnits : String -> Game -> Http.Request (List Unit)
requestUnits apiUrl game =
    let
        endpoint =
            apiUrl ++ "/units/" ++ (uuidToString <| getGameId game)
    in
    Http.get endpoint decodeUnits


requestCreatePlayer : String -> Game -> Http.Request Player
requestCreatePlayer apiUrl game =
    let
        endpoint =
            apiUrl ++ "/player"

        body =
            Http.jsonBody <| encodeGame game
    in
    Http.post endpoint body decodePlayer


requestCreateTurn : String -> Player -> Http.Request Turn
requestCreateTurn apiUrl player =
    let
        endpoint =
            apiUrl ++ "/turns"

        body =
            Http.jsonBody <| encodePlayer player
    in
    Http.post endpoint body decodeTurn
