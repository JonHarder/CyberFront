module Map exposing (Map, decodeMap, viewMap)

import Graphics exposing (Sprite, render, tileToSprite)
import Html.Styled exposing (Html, div, fromUnstyled, li, text, ul)
import Json.Decode as Decode exposing (Decoder, string)
import Json.Decode.Pipeline exposing (optional, required)
import Map.Tiles exposing (Terrain, Tile, decodeTile)
import Types exposing (Coord, Dimensions, Owner, decodeOwner)
import Unit exposing (Unit)
import Util exposing (enumerate)


type MapWidth
    = MapWidth Int


type alias MapInternals =
    { tiles : List Tile
    , width : MapWidth
    }


type Map
    = Map MapInternals


indexToCoord : Int -> MapWidth -> Coord
indexToCoord index (MapWidth width) =
    { x = modBy width index, y = index // width }


mapToSprites : Map -> List Sprite
mapToSprites (Map data) =
    let
        tiles : List ( Int, Tile )
        tiles =
            enumerate data.tiles

        toSprite ( index, tile ) =
            tileToSprite tile (indexToCoord index data.width)
    in
    List.map toSprite tiles


getMapDimensions : Map -> Dimensions
getMapDimensions (Map data) =
    let
        (MapWidth width) =
            data.width

        height =
            List.length data.tiles // width
    in
    { width = width, height = height }


viewMap : Map -> Html msg
viewMap map =
    let
        sprites =
            mapToSprites map

        dimensions =
            getMapDimensions map
    in
    fromUnstyled <| render dimensions sprites


decodeMap : Decoder Map
decodeMap =
    Decode.succeed MapInternals
        |> required "tiles" (Decode.list decodeTile)
        |> required "width" (Decode.int |> Decode.map MapWidth)
        |> Decode.map Map
