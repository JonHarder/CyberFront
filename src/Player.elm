module Player
    exposing
        ( Player
        , createPlayer
        , decodePlayerNumber
        , encodePlayer
        , getPlayer
        , getPlayerId
        , getPlayerNumber
        , showPlayer
        , yourTurn
        )

import Game exposing (Game, encodeGame)
import Html.Styled exposing (Html, div, text)
import Http
import Json.Decode as Decode exposing (Decoder, string)
import Json.Decode.Pipeline exposing (optional, required)
import Json.Encode as Encode exposing (Value)
import Types exposing (Uuid, decodeUuid, uuidToString)


type alias PlayerInternals =
    { id : Uuid
    , playerNumber : Int
    }


type Player
    = Player PlayerInternals


yourTurn : Player -> Int -> Bool
yourTurn (Player p) n =
    p.playerNumber == n


getPlayerId : Player -> Uuid
getPlayerId (Player data) =
    data.id


getPlayerNumber : Player -> Int
getPlayerNumber (Player data) =
    data.playerNumber


getPlayer : String -> String -> (Result Http.Error Player -> msg) -> Cmd msg
getPlayer apiUrl playerId toMsg =
    let
        endpoint =
            apiUrl ++ "/player/" ++ playerId

        request =
            Http.get endpoint decodePlayer
    in
    Http.send toMsg request


createPlayer : String -> Game -> (Result Http.Error Player -> msg) -> Cmd msg
createPlayer apiUrl game makeMsg =
    Http.send makeMsg (requestCreatePlayer apiUrl game)


requestCreatePlayer : String -> Game -> Http.Request Player
requestCreatePlayer apiUrl game =
    let
        endpoint =
            apiUrl ++ "/player"

        body =
            Http.jsonBody <| encodeGame game
    in
    Http.post endpoint body decodePlayer


decodePlayerNumber : Decoder Int
decodePlayerNumber =
    Decode.int


decodePlayer : Decoder Player
decodePlayer =
    Decode.succeed PlayerInternals
        |> required "id" decodeUuid
        |> required "playerNumber" Decode.int
        |> Decode.map Player


encodePlayer : Player -> Value
encodePlayer player =
    Encode.object [ ( "playerId", Encode.string (uuidToString <| getPlayerId player) ) ]


showPlayer : Player -> Html msg
showPlayer player =
    let
        playerId =
            getPlayerId player
    in
    div []
        [ div []
            [ text <| "Player: " ++ uuidToString playerId ]
        , div []
            [ text <| "num: " ++ String.fromInt (getPlayerNumber player) ]
        ]
