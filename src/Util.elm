module Util exposing (enumerate)


enumerate : List a -> List ( Int, a )
enumerate items =
    let
        combine x y =
            ( x, y )

        iter =
            List.range 0 (List.length items)
    in
    List.map2 combine iter items
