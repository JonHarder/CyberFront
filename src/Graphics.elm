module Graphics exposing (Sprite, render, tileToSprite)

import Html exposing (Html)
import Map.Tiles exposing (Tile, getSpriteName)
import Svg exposing (Svg, image, svg)
import Svg.Attributes exposing (..)
import Types exposing (Coord, Dimensions)


type Sprite
    = Sprite
        { spriteName : String
        , coord : Coord
        }


tileToSprite : Tile -> Coord -> Sprite
tileToSprite tile coord =
    Sprite
        { spriteName = getSpriteName tile
        , coord = coord
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
        [ width <| String.fromInt 640
        , height <| String.fromInt 640
        , vbox
        ]
        (List.map renderSprite sprites)
