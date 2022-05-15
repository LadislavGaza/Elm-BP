port module Ports exposing (..)

import Json.Decode as Decode


port storeUser : Decode.Value -> Cmd msg
