module Player exposing
    ( Player
    , createPlayer
    , encodePlayer
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


type alias PlayerNumber =
    Int


yourTurn : Player -> PlayerNumber -> Bool
yourTurn (Player p) playerNum =
    p.playerNumber == playerNum


getPlayerId : Player -> Uuid
getPlayerId (Player data) =
    data.id


getPlayerNumber : Player -> Int
getPlayerNumber (Player data) =
    data.playerNumber


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
