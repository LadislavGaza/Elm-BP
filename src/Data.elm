module Data exposing (..)

import Color exposing (..)
import Dict exposing (Dict)
import Dict.Extra as Extra


type alias User =
    { username : String
    , level : Int
    }


tileColor =
    lightGreen


roadColor =
    darkGray


blockSize =
    64


type alias Delta =
    Float


type Direction
    = Up
    | Right
    | Down
    | Left


type Movement
    = KeyInput
    | Animated


type Field
    = Road Car
    | RoadEmpty
    | Tile
    | Empty


type alias Point =
    ( Int, Int )


type alias Car =
    { movement : Movement
    , direction : Direction
    , color : Color.Color
    }


type alias Board =
    { boardItself : Dict Point Field, remainingJumps : Int, winningField : Point, won : Bool }
