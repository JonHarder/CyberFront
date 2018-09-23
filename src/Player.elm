module Player
    exposing
        ( Player
        , showPlayer
        , yourTurn
        )

import Html.Styled exposing (Html, div, text)
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
