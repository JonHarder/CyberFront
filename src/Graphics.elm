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


renderSprite : String -> Sprite -> Svg msg
renderSprite svgPath (Sprite sprite) =
    image
        [ xlinkHref <| svgPath ++ "#" ++ sprite.spriteName
        , x <| String.fromInt sprite.coord.x
        , y <| String.fromInt sprite.coord.y
        , width "1"
        , height "1"
        ]
        []


render : String -> Dimensions -> List Sprite -> Html msg
render svgPath dimensions sprites =
    let
        vbox =
            [ 0, 0, dimensions.width, dimensions.height ]
                |> List.map String.fromInt
                |> String.join " "
                |> viewBox
    in
    svg
        [ width <| String.fromInt 320
        , height <| String.fromInt 320
        , vbox
        ]
        (List.map (renderSprite svgPath) sprites)
