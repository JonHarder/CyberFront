module Main exposing (..)

import Api exposing (Game, Player, getGameId, requestCreatePlayer, requestGame, showGame, showPlayer)
import Browser exposing (Document)
import Css exposing (..)
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (css)
import Html.Styled.Events exposing (onClick)
import Http
import Json.Encode exposing (Value)
import Map exposing (..)
import Pusher exposing (joinGame, newTurn)
import Svg
import Svg.Attributes as SvgAttr


type alias PreLobbyData =
    { apiUrl : String, message : String, gameLoading : Maybe Game }


type alias LobbyData =
    { message : String
    , game : Game
    , player : Player
    , apiUrl : String
    }


type alias InGameData =
    { message : String
    , game : Game
    , player : Player
    , apiUrl : String
    , turnNumber : Int
    }


type Model
    = PreLobby PreLobbyData
    | Lobby LobbyData
    | InGame InGameData


type Msg
    = GotGame (Result Http.Error Game)
    | GotPlayer (Result Http.Error Player)
    | NewTurn Int


createGame : String -> Cmd Msg
createGame apiUrl =
    Http.send GotGame (requestGame apiUrl)


getPlayer : String -> Game -> Cmd Msg
getPlayer apiUrl game =
    Http.send GotPlayer (requestCreatePlayer apiUrl game)


init : String -> ( Model, Cmd Msg )
init apiUrl =
    ( PreLobby { apiUrl = apiUrl, message = "Pre lobby", gameLoading = Nothing }
    , createGame apiUrl
    )


updatePreLobby : Msg -> PreLobbyData -> ( Model, Cmd Msg )
updatePreLobby msg data =
    let
        model =
            PreLobby data
    in
    case msg of
        GotGame result ->
            case result of
                Err err ->
                    ( PreLobby { data | message = "failed to create game" }, Cmd.none )

                Ok game ->
                    ( PreLobby
                        { data
                            | message = "created game, creating player"
                            , gameLoading = Just game
                        }
                    , Cmd.batch [ joinGame game, getPlayer data.apiUrl game ]
                    )

        GotPlayer result ->
            case result of
                Err err ->
                    ( PreLobby { data | message = "failed to join game" }, Cmd.none )

                Ok player ->
                    case data.gameLoading of
                        Just game ->
                            ( Lobby
                                { message = "In Lobby. Waiting for more players"
                                , game = game
                                , player = player
                                , apiUrl = data.apiUrl
                                }
                            , Cmd.none
                            )

                        Nothing ->
                            ( PreLobby { data | message = "no reference to game :(" }, Cmd.none )

        _ ->
            ( model, Cmd.none )


updateLobby : Msg -> LobbyData -> ( Model, Cmd Msg )
updateLobby msg data =
    let
        model =
            Lobby data
    in
    case msg of
        NewTurn turnNumber ->
            ( InGame
                { message = "Game Started!"
                , game = data.game
                , player = data.player
                , turnNumber = turnNumber
                , apiUrl = data.apiUrl
                }
            , Cmd.none
            )

        _ ->
            ( model, Cmd.none )


updateInGame : Msg -> InGameData -> ( Model, Cmd Msg )
updateInGame msg data =
    ( InGame data, Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case model of
        PreLobby data ->
            updatePreLobby msg data

        Lobby data ->
            updateLobby msg data

        InGame data ->
            updateInGame msg data


preLobbyView : PreLobbyData -> List (Html Msg)
preLobbyView data =
    [ h1 [] [ text data.message ] ]


lobbyView : LobbyData -> List (Html Msg)
lobbyView data =
    [ h1 [] [ text "Lobby" ]
    , h2 [] [ text data.message ]
    , div
        [ css [ margin (px 30) ]
        ]
        [ showGame data.game ]
    , div [ css [ margin (px 30) ] ]
        [ showPlayer data.player
        ]
    ]


inGameView : InGameData -> List (Html Msg)
inGameView data =
    [ h1 [] [ text "In Game!" ]
    , h2 [] [ text data.message ]
    , div
        [ css [ margin (px 30) ]
        ]
        [ showGame data.game
        , showPlayer data.player
        , text <| "turn number: " ++ String.fromInt data.turnNumber
        ]
    ]


gameView : Model -> List (Html Msg)
gameView model =
    case model of
        PreLobby data ->
            preLobbyView data

        Lobby data ->
            lobbyView data

        InGame data ->
            inGameView data


view : Model -> Document Msg
view model =
    let
        gridDimensions =
            { width = 640
            , height = 480
            }

        sprites =
            [ Sprite
                { image =
                    { src = "http://www.placepuppy.net/64/64"
                    , dimensions = { width = 64, height = 64 }
                    }
                , coord =
                    { x = 0, y = 0 }
                }
            , Sprite
                { image =
                    { src = "http://www.placepuppy.net/64/64"
                    , dimensions = { width = 64, height = 64 }
                    }
                , coord =
                    { x = 64, y = 0 }
                }
            , Sprite
                { image =
                    { src = "http://www.placepuppy.net/64/64"
                    , dimensions = { width = 64, height = 64 }
                    }
                , coord =
                    { x = 64, y = 128 }
                }
            ]
    in
    { title = "CyberWars"

    -- , body = List.map toUnstyled <| gameView model
    , body =
        [ toUnstyled <|
            grid gridDimensions sprites
        ]
    }


subscriptions : Model -> Sub Msg
subscriptions model =
    case model of
        Lobby _ ->
            newTurn NewTurn

        _ ->
            Sub.none



-- main : Program String Model Msg


type alias Image =
    { src : String
    , dimensions : Dimensions
    }


type alias Dimensions =
    { width : Int
    , height : Int
    }


type alias Coord =
    { x : Int
    , y : Int
    }


type Sprite
    = Sprite
        { image : Image
        , coord : Coord
        }


sprite : Sprite -> Svg.Svg msg
sprite (Sprite { image, coord }) =
    Svg.image
        [ SvgAttr.x <| String.fromInt coord.x
        , SvgAttr.y <| String.fromInt coord.y
        , SvgAttr.width <| String.fromInt image.dimensions.width
        , SvgAttr.height <| String.fromInt image.dimensions.height
        , SvgAttr.xlinkHref image.src
        ]
        []


grid : Dimensions -> List Sprite -> Html msg
grid dimensions sprites =
    let
        background =
            Svg.rect
                [ SvgAttr.x "0"
                , SvgAttr.y "0"
                , SvgAttr.width <| String.fromInt dimensions.width
                , SvgAttr.height <| String.fromInt dimensions.height
                , SvgAttr.fill "#8d8d8d"
                ]
                []
    in
    fromUnstyled <|
        Svg.svg
            [ SvgAttr.width <| String.fromInt dimensions.width
            , SvgAttr.height <| String.fromInt dimensions.height
            , SvgAttr.viewBox <| "0 0 " ++ String.fromInt dimensions.width ++ " " ++ String.fromInt dimensions.height
            ]
            (background :: List.map sprite sprites)


main : Program String Model Msg
main =
    Browser.document
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }
