module Unit exposing (Unit, decodeUnits, getUnits)

import Game exposing (Game, getGameId)
import Http
import Json.Decode as Decode exposing (Decoder, string)
import Json.Decode.Pipeline exposing (required)
import Types
    exposing
        ( Coord
        , Owner
        , Uuid
        , decodeCoord
        , decodeOwner
        , decodeUuid
        , uuidToString
        )


type UnitType
    = Magic
    | Swords
    | Guns


typeDamageModifier : UnitType -> UnitType -> Float
typeDamageModifier t1 t2 =
    case ( t1, t2 ) of
        ( Magic, Guns ) ->
            2.0

        ( Guns, Swords ) ->
            2.0

        ( Swords, Magic ) ->
            2.0

        ( Magic, Swords ) ->
            0.5

        ( Swords, Guns ) ->
            0.5

        ( Guns, Magic ) ->
            0.5

        ( Magic, Magic ) ->
            1.0

        ( Guns, Guns ) ->
            1.0

        ( Swords, Swords ) ->
            1.0


type alias UnitInternals =
    { id : Uuid
    , attack : Int
    , defence : Int
    , health : Int
    , unitType : UnitType
    , minRange : Int
    , maxRange : Int
    , speed : Int
    , coord : Maybe Coord
    , owner : Owner
    , currentAP : Int
    , maxAP : Int
    , apRegen : Int
    }


type Unit
    = Unit UnitInternals


getUnits : String -> Game -> (Result Http.Error (List Unit) -> msg) -> Cmd msg
getUnits apiUrl game makeMsg =
    let
        endpoint =
            apiUrl ++ "/units/" ++ (uuidToString <| getGameId game)

        request =
            Http.get endpoint decodeUnits
    in
    Http.send makeMsg request


decodeUnit : Decoder Unit
decodeUnit =
    Decode.succeed UnitInternals
        |> required "id" decodeUuid
        |> required "attack" Decode.int
        |> required "defense" Decode.int
        |> required "health" Decode.int
        |> required "unitType" (Decode.string |> Decode.andThen unitTypeHelper)
        |> required "minRange" Decode.int
        |> required "maxRange" Decode.int
        |> required "speed" Decode.int
        |> required "coordinate" (Decode.nullable decodeCoord)
        |> required "owner" (Decode.int |> Decode.andThen decodeOwner)
        |> required "currentAP" Decode.int
        |> required "maxAP" Decode.int
        |> required "apRegen" Decode.int
        |> Decode.map Unit


decodeUnits : Decoder (List Unit)
decodeUnits =
    Decode.list decodeUnit


unitTypeHelper : String -> Decoder UnitType
unitTypeHelper unitType =
    case unitType of
        "magic" ->
            Decode.succeed Magic

        "swords" ->
            Decode.succeed Swords

        "guns" ->
            Decode.succeed Guns

        _ ->
            Decode.fail <| "Trying to decode a unit type, but type: " ++ unitType ++ " is not supported."
