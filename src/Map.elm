module Map exposing (..)

import Unit exposing (Unit)


type Terrain
    = Walkable


type Tile
    = Tile
        { terrain : Terrain
        }


type alias MapInternals =
    { tiles : List Tile
    , units : List (Maybe Unit)
    }


type Map
    = Map MapInternals
