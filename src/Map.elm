module Map exposing (..)

import Unit exposing (Owner, Unit)


type alias TerrainProperties =
    { walkable : Bool
    , combatMultiplier : Float
    }


concrete : Terrain
concrete =
    Terrain
        { walkable = True
        , combatMultiplier = 1.0
        }


getMapWidth : Map -> Int
getMapWidth (Map map) =
    map.width


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
    , units : List Unit
    , width : Int
    }


type Map
    = Map MapInternals
