module States.Loading exposing (..)

import Config exposing (Config)
import Game exposing (Game, showGame)
import Html.Styled as Html exposing (Html)
import Http
import Player exposing (Player)
import Session exposing (saveSession)


type Msg
    = GotPlayer (Result Http.Error Player)


type alias Model =
    { game : Game }


update : Msg -> Model -> (String -> model) -> (String -> Game -> Player -> model) -> ( model, Cmd msg )
update msg model toCurrentState toNextState =
    case msg of
        GotPlayer response ->
            case response of
                Ok player ->
                    ( toNextState "Logged in!" model.game player
                    , saveSession { game = model.game, player = player }
                    )

                Err _ ->
                    ( toCurrentState "Failed to log in :("
                    , Cmd.none
                    )


view : Config -> Model -> (Msg -> msg) -> Html msg
view config model toMsg =
    showGame config.svgPath model.game
        |> Html.map toMsg
