module Map exposing (..)

import Html.Styled exposing (Html, div, li, text, ul)
import Unit exposing (Owner, Unit)


type alias TerrainProperties =
    { walkable : Bool
    , combatMultiplier : Float
    , description : String
    }


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
