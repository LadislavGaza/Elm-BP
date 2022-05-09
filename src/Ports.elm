port module Ports exposing (..)

import Data exposing (..)
import Json.Decode as Decode
import Json.Encode as Encode


port storeUser : Decode.Value -> Cmd msg


port play : Encode.Value -> Cmd msg
