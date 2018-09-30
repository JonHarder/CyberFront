port module Ports exposing (..)

import Json.Encode exposing (Value)


port savePhaseData : Value -> Cmd msg
