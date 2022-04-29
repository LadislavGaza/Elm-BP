-- Walk around with the arrow keys. Press the UP arrow to jump!
--
-- Learn more about the playground here:
--   https://package.elm-lang.org/packages/evancz/elm-playground/latest/
--


module Pages.PlaygroundFail2 exposing (main, update, view)

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


main =
    game view
        update
        { x = 0
        , y = 0
        , angle = 90
        }



-- VIEW


view computer turtle =
    let
        car1 =
            move 150 150 (rotate 180 (image 68 68 "../car.png"))

        car2 =
            move 300 300 (rotate 180 (image 68 68 "../car.png"))
    in
    [ image computer.screen.width
        computer.screen.height
        "../map.webp"
    , if computer.keyboard.space == True then
        car2
            |> move turtle.x turtle.y
            |> rotate turtle.angle

      else
        car1
            |> move turtle.x turtle.y
            |> rotate turtle.angle
    ]


update computer turtle =
    let
        x =
            if computer.keyboard.up then
                turtle.x + 4 * toY computer.keyboard * cos (degrees turtle.angle)

            else
                turtle.x

        y =
            if computer.keyboard.up then
                turtle.y + 4 * toY computer.keyboard * sin (degrees turtle.angle)

            else
                turtle.y
    in
    { x = x
    , y = y
    , angle = turtle.angle - toX computer.keyboard * 2
    }
