module Game exposing (Game, createGame, currentPlayer, encodeGame, getGame, getGameId, showGame, updatePlayerNumber)

import Css exposing (marginBottom, px)
import Html.Styled exposing (Html, div, text)
import Html.Styled.Attributes exposing (css)
import Http
import Json.Decode as Decode exposing (Decoder, string)
import Json.Decode.Pipeline exposing (optional, required)
import Json.Encode as Encode exposing (Value)
import Map exposing (Map, decodeMap, viewMap)
import Types exposing (Uuid, decodeUuid, uuidToString)


type alias GameInternals =
    { id : Uuid
    , map : Map
    , playerNumber : Int
    }


type Game
    = Game GameInternals


currentPlayer : Game -> Int
currentPlayer (Game data) =
    data.playerNumber


updatePlayerNumber : Game -> Int -> Game
updatePlayerNumber (Game data) playerNumber =
    Game { data | playerNumber = playerNumber }


showGame : String -> Game -> Html msg
showGame svgPath (Game game) =
    div []
        [ viewMap svgPath game.map ]


getGameId : Game -> Uuid
getGameId (Game data) =
    data.id


getGame : String -> String -> (Result Http.Error Game -> msg) -> Cmd msg
getGame apiUrl gameId makeMsg =
    let
        endpoint =
            apiUrl ++ "/game/" ++ gameId

        request =
            Http.get endpoint decodeGame
    in
    Http.send makeMsg request


requestGame : String -> Http.Request Game
requestGame apiUrl =
    let
        endpoint =
            apiUrl ++ "/game"
    in
    Http.post endpoint Http.emptyBody decodeGame


createGame : String -> (Result Http.Error Game -> msg) -> Cmd msg
createGame apiUrl makeMsg =
    Http.send makeMsg (requestGame apiUrl)


decodeGame : Decoder Game
decodeGame =
    Decode.succeed GameInternals
        |> required "id" decodeUuid
        |> required "map" decodeMap
        |> required "playerNumber" Decode.int
        |> Decode.map Game


encodeGame : Game -> Value
encodeGame game =
    Encode.object [ ( "gameId", Encode.string (uuidToString <| getGameId game) ) ]
