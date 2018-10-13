port module Ports exposing (..)

import Json.Encode exposing (Value)


port savePhaseData : Value -> Cmd msg


port resetGame : () -> Cmd msg
