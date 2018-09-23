module TileMap exposing (Sprite(..), TileName(..), render)

import Html exposing (Html)
import Svg exposing (Svg, defs, image, svg, use)
import Svg.Attributes exposing (..)
import Types exposing (Coord, Dimensions)


type TileName
    = Concrete
    | Brick


type Sprite
    = Sprite
        { spriteName : String
        , coord : Coord
        }


renderSprite : Sprite -> Svg msg
renderSprite (Sprite sprite) =
    image
        [ xlinkHref <| "assets/spritemap.svg#" ++ sprite.spriteName
        , x <| String.fromInt sprite.coord.x
        , y <| String.fromInt sprite.coord.y
        , width "1"
        , height "1"
        ]
        []


render : Dimensions -> List Sprite -> Html msg
render dimensions sprites =
    let
        vbox =
            [ 0, 0, dimensions.width, dimensions.height ]
                |> List.map String.fromInt
                |> String.join " "
                |> viewBox
    in
    svg
        [ width <| String.fromInt 80
        , height <| String.fromInt 80
        , vbox
        ]
        (List.map renderSprite sprites)
