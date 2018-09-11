module Api exposing (Game, Player, Uuid, getGameId, requestCreatePlayer, requestGame, showGame, showPlayer)

import Html.Styled exposing (Html, div, text)
import Http
import Json.Decode as Decode exposing (Decoder, string)
import Json.Decode.Pipeline exposing (hardcoded, optional, required)
import Json.Encode as Encode exposing (Value)
import Map exposing (..)
import Unit exposing (..)



-- TODO: use elm/url Url builders


type Uuid
    = Uuid String


type alias GameInternals =
    { id : Uuid
    , map : Map
    }


type Game
    = Game GameInternals


type alias PlayerInternals =
    { id : Uuid
    , playerNumber : Int
    }


type Player
    = Player PlayerInternals


showPlayer : Player -> Html msg
showPlayer (Player player) =
    let
        (Uuid id) =
            player.id
    in
    div []
        [ div []
            [ text <| "Player: " ++ id ]
        , div []
            [ text <| "num: " ++ String.fromInt player.playerNumber ]
        ]


getGameId : Game -> String
getGameId (Game game) =
    let
        (Uuid id) =
            game.id
    in
    id


showGame : Game -> Html msg
showGame (Game game) =
    let
        (Uuid id) =
            game.id
    in
    div []
        [ div []
            [ text <| "Game " ++ id ]
        , showMap game.map
        ]


decodeUuid : Decoder Uuid
decodeUuid =
    Decode.map Uuid string


decodeGame : Decoder Game
decodeGame =
    Decode.succeed GameInternals
        |> required "id" decodeUuid
        |> required "map" decodeMap
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


tileHelper : String -> Decoder Terrain
tileHelper terrainType =
    case terrainType of
        "concrete" ->
            Decode.succeed concrete

        _ ->
            Decode.fail <| "Trying to decode a terrain type, but type: " ++ terrainType ++ " is not supported."


decodeTile : Decoder Tile
decodeTile =
    Decode.succeed TileInternals
        |> required "type" (Decode.string |> Decode.andThen tileHelper)
        |> required "owner" (Decode.int |> Decode.andThen ownerDecoder)
        |> Decode.map Tile


ownerDecoder : Int -> Decoder Owner
ownerDecoder playerNum =
    case playerNum of
        0 ->
            Decode.succeed Nothing

        1 ->
            Decode.succeed (Just 1)

        2 ->
            Decode.succeed (Just 2)

        _ ->
            Decode.fail <| "Bad player number " ++ String.fromInt playerNum


decodeUnit : Decoder Unit
decodeUnit =
    Decode.succeed UnitInternals
        |> required "attack" Decode.int
        |> required "defense" Decode.int
        |> required "health" Decode.int
        |> required "unitType" (Decode.string |> Decode.andThen unitTypeHelper)
        |> required "minRange" Decode.int
        |> required "maxRange" Decode.int
        |> required "speed" Decode.int
        |> required "coordinate" (Decode.nullable coordDecoder)
        |> required "owner" (Decode.int |> Decode.andThen ownerDecoder)
        |> Decode.map Unit


coordDecoder : Decoder Coord
coordDecoder =
    Decode.succeed Coord
        |> required "x" Decode.int
        |> required "y" Decode.int


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


decodeMap : Decoder Map
decodeMap =
    Decode.succeed MapInternals
        |> required "tiles" (Decode.list decodeTile)
        |> required "width" Decode.int
        |> Decode.map Map


requestGame : String -> Http.Request Game
requestGame apiUrl =
    let
        endpoint =
            apiUrl ++ "/game"
    in
    Http.post endpoint Http.emptyBody decodeGame


requestCreatePlayer : String -> Game -> Http.Request Player
requestCreatePlayer apiUrl game =
    let
        endpoint =
            apiUrl ++ "/player"

        payload =
            encodeGame game

        body =
            Http.jsonBody payload
    in
    Http.post endpoint body decodePlayer
