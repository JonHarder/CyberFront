module Unit exposing (Unit, decodeUnits, detailedUnitView, getUnits, unitCoordinates, unitOwner, viewUnit)

import Css exposing (..)
import Game exposing (Game, getGameId)
import Html.Styled exposing (Html, div, li, span, text, ul)
import Html.Styled.Attributes exposing (css)
import Html.Styled.Events exposing (onClick)
import Http
import Json.Decode as Decode exposing (Decoder, string)
import Json.Decode.Pipeline exposing (required)
import Types
    exposing
        ( Coord
        , Owner
        , Uuid
        , decodeCoord
        , decodeOwner
        , decodeUuid
        , uuidToString
        )


type UnitType
    = Magic
    | Swords
    | Guns


showUnitType : UnitType -> String
showUnitType unitType =
    case unitType of
        Magic ->
            "magic"

        Swords ->
            "swords"

        Guns ->
            "guns"


typeDamageModifier : UnitType -> UnitType -> Float
typeDamageModifier t1 t2 =
    case ( t1, t2 ) of
        ( Magic, Guns ) ->
            2.0

        ( Guns, Swords ) ->
            2.0

        ( Swords, Magic ) ->
            2.0

        ( Magic, Swords ) ->
            0.5

        ( Swords, Guns ) ->
            0.5

        ( Guns, Magic ) ->
            0.5

        ( Magic, Magic ) ->
            1.0

        ( Guns, Guns ) ->
            1.0

        ( Swords, Swords ) ->
            1.0


type alias UnitInternals =
    { id : Uuid
    , attack : Int
    , defence : Int
    , health : Int
    , unitType : UnitType
    , minRange : Int
    , maxRange : Int
    , speed : Int
    , coord : Maybe Coord
    , owner : Owner
    , currentAP : Int
    , maxAP : Int
    , apRegen : Int
    }


type Unit
    = Unit UnitInternals


detailedUnitView : (Unit -> msg) -> Unit -> Html msg
detailedUnitView clickUnit (Unit data) =
    ul []
        [ li [] [ text ("attack: " ++ String.fromInt data.attack) ]
        , li [] [ text ("defence: " ++ String.fromInt data.defence) ]
        , li [] [ text ("health: " ++ String.fromInt data.health) ]
        , li [] [ text ("type: " ++ showUnitType data.unitType) ]
        , li [] [ text ("speed: " ++ String.fromInt data.speed) ]
        , li [] [ text ("ap: " ++ String.fromInt data.currentAP) ]
        ]


viewUnit : (Unit -> msg) -> Unit -> Html msg
viewUnit clickUnit unit =
    let
        (Unit data) =
            unit
    in
    div [ css [ hover [ cursor pointer ] ], onClick (clickUnit unit) ] [ text <| showUnitType data.unitType ++ " unit" ]


unitCoordinates : Unit -> Maybe Coord
unitCoordinates (Unit data) =
    data.coord


unitOwner : Unit -> Owner
unitOwner (Unit data) =
    data.owner


getUnits : String -> Game -> (Result Http.Error (List Unit) -> msg) -> Cmd msg
getUnits apiUrl game makeMsg =
    let
        endpoint =
            apiUrl ++ "/units/" ++ (uuidToString <| getGameId game)

        request =
            Http.get endpoint decodeUnits
    in
    Http.send makeMsg request


decodeUnit : Decoder Unit
decodeUnit =
    Decode.succeed UnitInternals
        |> required "id" decodeUuid
        |> required "attack" Decode.int
        |> required "defense" Decode.int
        |> required "health" Decode.int
        |> required "unitType" (Decode.string |> Decode.andThen unitTypeHelper)
        |> required "minRange" Decode.int
        |> required "maxRange" Decode.int
        |> required "speed" Decode.int
        |> required "coordinates" (Decode.nullable decodeCoord)
        |> required "owner" (Decode.int |> Decode.andThen decodeOwner)
        |> required "currentAP" Decode.int
        |> required "maxAP" Decode.int
        |> required "apRegen" Decode.int
        |> Decode.map Unit


decodeUnits : Decoder (List Unit)
decodeUnits =
    Decode.list decodeUnit


unitTypeHelper : String -> Decoder UnitType
unitTypeHelper unitType =
    case unitType of
        "magic" ->
            Decode.succeed Magic

        "swords" ->
            Decode.succeed Swords

        "guns" ->
            Decode.succeed Guns

        _ ->
            Decode.fail <| "Trying to decode a unit type, but type: " ++ unitType ++ " is not supported."
