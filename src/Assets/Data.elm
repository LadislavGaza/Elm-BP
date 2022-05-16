module Assets.Data exposing (..)

import Color exposing (..)
import Dict exposing (Dict)



-- defined types and constants


type alias User =
    { extraJumps : Int
    , extraGameSpeed : Int
    , extraDuration : Int
    , level1HS : Int
    , level2HS : Int
    , level3HS : Int
    }


tileColor =
    green


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
