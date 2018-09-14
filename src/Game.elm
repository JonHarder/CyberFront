module Game exposing (Game, decodeGame, encodeGame, getGameId, showGame)

import Html.Styled exposing (Html, div, text)
import Json.Decode as Decode exposing (Decoder, string)
import Json.Decode.Pipeline exposing (optional, required)
import Json.Encode as Encode exposing (Value)
import Map exposing (Map, decodeMap, showMap)
import Types exposing (Uuid, decodeUuid, uuidToString)


type alias GameInternals =
    { id : Uuid
    , map : Map
    }


type Game
    = Game GameInternals


showGame : Game -> Html msg
showGame (Game game) =
    let
        id =
            game.id
    in
    div []
        [ div []
            [ text <| "Game " ++ uuidToString id ]
        , showMap game.map
        ]


getGameId : Game -> Uuid
getGameId (Game data) =
    data.id


decodeGame : Decoder Game
decodeGame =
    Decode.succeed GameInternals
        |> required "id" decodeUuid
        |> required "map" decodeMap
        |> Decode.map Game


encodeGame : Game -> Value
encodeGame game =
    Encode.object [ ( "gameId", Encode.string (uuidToString <| getGameId game) ) ]
