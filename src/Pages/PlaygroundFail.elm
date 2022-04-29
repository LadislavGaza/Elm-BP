-- Walk around with the arrow keys. Press the UP arrow to jump!
--
-- Learn more about the playground here:
--   https://package.elm-lang.org/packages/evancz/elm-playground/latest/
--


module Pages.PlaygroundFail exposing (main, update, view)

import Array
import Browser
import Html exposing (Attribute, Html, button, div, input, text)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput)
import Main exposing (Msg)
import Playground exposing (..)


type Msg
    = Move1
    | Move2
    | Change



-- MAIN


pomoc =
    Array.fromList
        [ { x = 0
          , y = 0
          , angle = 90
          }
        , { x = 0
          , y = 0
          , angle = 90
          }
        ]


main =
    game view
        update
        pomoc



-- VIEW


view computer bruh =
    let
        car1 =
            move 150 150 (rotate 180 (image 68 68 "../car.png"))
                |> move .x .y
                |> rotate .angle

        car2 =
            move 300 300 (rotate 180 (image 68 68 "../car.png"))
                |> move .x .y
                |> rotate .angle

        list =
            Array.fromList
                [ car1, car2 ]
    in
    [ image computer.screen.width computer.screen.height "../map.webp"

    -- , if computer.keyboard.space then
    --     car2
    --
    --   else
    --     car1
    --         |> move turtle1.x turtle1.y
    --         |> rotate turtle1.angle
    , car1
    , car2
    , list
    ]


update computer bruh =
    let
        car1 =
            Array.get 1 list

        car2 =
            Array.get 2 list

        x =
            if computer.keyboard.up then
                car1.x + 4 * toY computer.keyboard * cos (degrees car1.angle)

            else
                car1.x

        y =
            if computer.keyboard.up then
                car1.y + 4 * toY computer.keyboard * sin (degrees car1.angle)

            else
                car1.y
    in
    { x = x
    , y = y
    , angle = car1.angle - toX computer.keyboard * 2
    }
