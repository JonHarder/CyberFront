module Graphics exposing (..)

import Html.Styled exposing (Html, fromUnstyled)
import Svg exposing (Svg)
import Svg.Attributes exposing (..)
import Types exposing (Coord, Dimensions)


type alias Image =
    { src : String
    , dimensions : Dimensions
    }


type Sprite
    = Sprite
        { image : Image
        , coord : Coord
        }


sprite : Sprite -> Svg msg
sprite (Sprite { image, coord }) =
    Svg.image
        [ x <| String.fromInt coord.x
        , y <| String.fromInt coord.y
        , width <| String.fromInt image.dimensions.width
        , height <| String.fromInt image.dimensions.height
        , xlinkHref image.src
        ]
        []


grid : Dimensions -> List Sprite -> Html msg
grid dimensions sprites =
    let
        viewBoxString =
            "0 0 "
                ++ String.fromInt dimensions.width
                ++ " "
                ++ String.fromInt dimensions.height
    in
    fromUnstyled <|
        Svg.svg
            [ width <| String.fromInt dimensions.width
            , height <| String.fromInt dimensions.height
            , viewBox viewBoxString
            ]
            (List.map sprite sprites)
