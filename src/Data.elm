module Data exposing (..)

import Color exposing (..)
import Dict exposing (Dict)
import Dict.Extra as Extra


type alias User =
    { username : String
    , level : Int
    }
