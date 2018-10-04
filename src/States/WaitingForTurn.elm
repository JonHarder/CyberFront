module States.WaitingForTurn exposing (..)

import Config exposing (Config)
import Game exposing (Game, showGame)
import Html.Styled as Html exposing (Html, h1, text)
import Player exposing (Player)
import Unit exposing (Unit)


type Msg
    = NoOp


type alias Model =
    { game : Game
    , player : Player
    , playerNumber : Int
    , units : List Unit
    }


view : Config -> Model -> (Msg -> msg) -> Html msg
view config model toMsg =
    showGame config.svgPath model.game
