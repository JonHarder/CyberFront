module Map exposing (Map, decodeMap, viewMap)

import Html.Styled exposing (Html, div, fromUnstyled, li, text, ul)
import Json.Decode as Decode exposing (Decoder, string)
import Json.Decode.Pipeline exposing (optional, required)
import TileMap exposing (Sprite(..), TileName(..), render)
import Types exposing (Coord, Dimensions, Owner, decodeOwner)
import Unit exposing (Unit)


type alias TerrainProperties =
    { walkable : Bool
    , combatMultiplier : Float
    , spriteName : String
    }


type Terrain
    = Terrain TerrainProperties


type alias TileInternals =
    { terrain : Terrain
    , owner : Owner
    }


type Tile
    = Tile TileInternals


tileToSprite : Tile -> Coord -> Sprite
tileToSprite (Tile tile) coord =
    let
        (Terrain terrain) =
            tile.terrain
    in
    Sprite
        { spriteName = terrain.spriteName
        , coord = coord
        }


indexToCoord : Int -> Int -> Coord
indexToCoord index width =
    { x = modBy width index, y = index // width }


enumerate : List a -> List ( Int, a )
enumerate items =
    let
        combine x y =
            ( x, y )

        iter =
            List.range 0 (List.length items)
    in
    List.map2 combine iter items


mapToSprites : Map -> List Sprite
mapToSprites (Map data) =
    let
        tiles =
            enumerate data.tiles

        toSprite ( index, tile ) =
            tileToSprite tile (indexToCoord index data.width)
    in
    List.map toSprite tiles


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
        , spriteName = "concrete"
        }


brick : Terrain
brick =
    Terrain
        { walkable = True
        , combatMultiplier = 1.0
        , spriteName = "brick"
        }


getMapDimensions : Map -> Dimensions
getMapDimensions (Map data) =
    let
        height =
            List.length data.tiles // data.width
    in
    { width = data.width, height = height }


getMapWidth : Map -> Int
getMapWidth (Map map) =
    map.width


viewMap : Map -> Html msg
viewMap map =
    let
        tiles =
            mapToSprites map
    in
    fromUnstyled <| render (getMapDimensions map) tiles


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

        "brick" ->
            Decode.succeed brick

        _ ->
            Decode.fail <| "Trying to decode a terrain type, but type: " ++ terrainType ++ " is not supported."
