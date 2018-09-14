module Map exposing (Map, decodeMap, showMap)

import Html.Styled exposing (Html, div, li, text, ul)
import Json.Decode as Decode exposing (Decoder, string)
import Json.Decode.Pipeline exposing (optional, required)
import Types exposing (Owner, decodeOwner)
import Unit exposing (Unit)


type alias TerrainProperties =
    { walkable : Bool
    , combatMultiplier : Float
    , description : String
    }


type Terrain
    = Terrain TerrainProperties


type alias TileInternals =
    { terrain : Terrain
    , owner : Owner
    }


type Tile
    = Tile TileInternals


type alias MapInternals =
    { tiles : List Tile
    , width : Int
    }


type Map
    = Map MapInternals


concrete : Terrain
concrete =
    Terrain
        { walkable = True
        , combatMultiplier = 1.0
        , description = "concrete"
        }


getMapWidth : Map -> Int
getMapWidth (Map map) =
    map.width


showTile : Tile -> Html msg
showTile (Tile tile) =
    let
        (Terrain data) =
            tile.terrain

        walkable =
            case data.walkable of
                True ->
                    "yes"

                False ->
                    "no"
    in
    li []
        [ div []
            [ text <| "terrain: " ++ data.description ]
        , div []
            [ text <| "walkable: " ++ walkable ]
        , div []
            [ text <| "combat mulitplier: " ++ String.fromFloat data.combatMultiplier ]
        ]


showMap : Map -> Html msg
showMap map =
    let
        (Map data) =
            map
    in
    div []
        [ div []
            [ text <| "Map (width " ++ String.fromInt (getMapWidth map) ++ ")" ]
        , div []
            [ text <| "tiles: " ++ String.fromInt (List.length data.tiles) ]
        , ul []
            (List.map showTile data.tiles)
        ]


decodeMap : Decoder Map
decodeMap =
    Decode.succeed MapInternals
        |> required "tiles" (Decode.list decodeTile)
        |> required "width" Decode.int
        |> Decode.map Map


decodeTile : Decoder Tile
decodeTile =
    Decode.succeed TileInternals
        |> required "type" (Decode.string |> Decode.andThen tileHelper)
        |> required "owner" (Decode.int |> Decode.andThen decodeOwner)
        |> Decode.map Tile


tileHelper : String -> Decoder Terrain
tileHelper terrainType =
    case terrainType of
        "concrete" ->
            Decode.succeed concrete

        _ ->
            Decode.fail <| "Trying to decode a terrain type, but type: " ++ terrainType ++ " is not supported."
