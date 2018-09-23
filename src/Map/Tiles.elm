module Map.Tiles
    exposing
        ( Terrain
        , Tile
        , brick
        , concrete
        , decodeTile
        , getSpriteName
        )

import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline exposing (required)
import Types exposing (Coord, Owner, decodeOwner)


type Terrain
    = Terrain TerrainProperties


type alias TerrainProperties =
    { walkable : Bool
    , combatMultiplier : Float
    , spriteName : String
    }


type alias TileInternals =
    { terrain : Terrain
    , owner : Owner
    }


type Tile
    = Tile TileInternals


getSpriteName : Tile -> String
getSpriteName (Tile tileData) =
    let
        (Terrain terrainData) =
            tileData.terrain
    in
    terrainData.spriteName


concrete : Terrain
concrete =
    Terrain
        { walkable = True
        , combatMultiplier = 1.0
        , spriteName = "concrete"
        }


brick : Terrain
brick =
    Terrain
        { walkable = True
        , combatMultiplier = 1.0
        , spriteName = "brick"
        }


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

        "brick" ->
            Decode.succeed brick

        _ ->
            Decode.fail <| "Trying to decode a terrain type, but type: " ++ terrainType ++ " is not supported."
