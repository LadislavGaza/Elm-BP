port module Ports exposing (..)

import Json.Decode as Decode



-- Javascript interop


port storeUser : Decode.Value -> Cmd msg
