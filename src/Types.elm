module Types
    exposing
        ( Coord
        , Dimensions
        , Owner
        , Uuid
        , decodeCoord
        , decodeOwner
        , decodeUuid
        , uuidToString
        )

import Json.Decode as Decode exposing (Decoder, string)
import Json.Decode.Pipeline exposing (required)


type Uuid
    = Uuid String


type alias PlayerNumber =
    Int


type alias Owner =
    Maybe PlayerNumber


decodeOwner : Int -> Decoder Owner
decodeOwner playerNum =
    case playerNum of
        0 ->
            Decode.succeed Nothing

        1 ->
            Decode.succeed (Just 1)

        2 ->
            Decode.succeed (Just 2)

        _ ->
            Decode.fail <| "Bad player number " ++ String.fromInt playerNum


decodeCoord : Decoder Coord
decodeCoord =
    Decode.succeed Coord
        |> required "x" Decode.int
        |> required "y" Decode.int


uuidToString : Uuid -> String
uuidToString (Uuid uuid) =
    uuid


decodeUuid : Decoder Uuid
decodeUuid =
    Decode.map Uuid string


type alias Dimensions =
    { width : Int
    , height : Int
    }


type alias Coord =
    { x : Int
    , y : Int
    }
