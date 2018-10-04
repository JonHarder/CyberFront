module Config exposing (..)

import Session exposing (Session)


type alias Config =
    { apiUrl : String
    , svgPath : String
    , session : Maybe Session
    }
