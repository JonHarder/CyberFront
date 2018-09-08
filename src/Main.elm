module Main exposing (..)

import Api exposing (Game, requestGame, showGame)
import Browser exposing (Document)
import Css exposing (..)
import Html
import Html.Styled exposing (..)
import Html.Styled.Events exposing (onClick)
import Http
import Json.Encode exposing (Value)


type alias Model =
    { game : Maybe Game
    , message : String
    }


type Msg
    = GetGame
    | GotGame (Result Http.Error Game)


init : flags -> ( Model, Cmd Msg )
init _ =
    ( { game = Nothing, message = "Initialized" }, Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GetGame ->
            ( model, Http.send GotGame requestGame )

        GotGame result ->
            case result of
                Err _ ->
                    let
                        _ =
                            Debug.log "you dun fucked up" ""
                    in
                    ( { model | message = "game creation failed" }, Cmd.none )

                Ok game ->
                    ( { model | game = Just game }, Cmd.none )


gameView : Model -> List (Html Msg)
gameView model =
    let
        gameString =
            case model.game of
                Just g ->
                    showGame g

                Nothing ->
                    "no game"
    in
    [ h1 [] [ text ("Message: " ++ model.message) ]
    , div []
        [ text gameString ]
    , button [ onClick GetGame ]
        [ text "Get Game" ]
    ]


view : Model -> Document Msg
view model =
    { title = "CyberWars"
    , body = List.map toUnstyled <| gameView model
    }


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


main : Program Value Model Msg
main =
    Browser.document
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }
