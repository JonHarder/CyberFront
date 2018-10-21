module States.Lobby exposing (..)

import Config exposing (Config)
import Game exposing (Game, getGameId, showGame)
import Html.Styled as Html exposing (Html, div, h3, text, input)
import Html.Styled.Attributes exposing (readonly, value)
import Player exposing (Player)
import Types exposing (uuidToString)


type Msg
    = NoOp


type alias Model =
    { game : Game
    , player : Player
    }


view : Config -> Model -> (Msg -> msg) -> Html msg
view config model toMsg =
    let
        gameId =
            uuidToString (getGameId model.game)
    in
    div []
        [ h3 [] [ text <| "join link: "]
        , input [ readonly True, value <| "http://localhost:1234/?gameId=" ++ gameId ] [ ]
        , showGame config.svgPath model.game
            |> Html.map toMsg
        ]
