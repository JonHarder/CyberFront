module Util exposing (enumerate, pluralize)


enumerate : List a -> List ( Int, a )
enumerate items =
    let
        combine x y =
            ( x, y )

        iter =
            List.range 0 (List.length items)
    in
    List.map2 combine iter items


pluralize : String -> String -> Int -> String
pluralize singular plural number =
    if number == 1 then
        singular

    else
        plural
