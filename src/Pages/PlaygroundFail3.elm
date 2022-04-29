module Pages.PlaygroundFail3 exposing (main, update, view)

import Playground exposing (..)


main =
    game view update ( 0, 0 )


view computer ( x, y ) =
    helperview2 ( x, y )


update computer ( x, y ) =
    ( x + toX computer.keyboard
    , y + toY computer.keyboard
    )


helperview ( x, y ) =
    [ square blue 40
        |> move x y
    , move 50 50 (square red 40)
        |> move x y
    ]


helperview2 ( x, y ) =
    [ square blue 40
        |> move x y
    , move 50 50 (square red 40)
    ]
