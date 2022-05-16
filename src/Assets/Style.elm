module Assets.Style exposing (..)

import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font



--this style is applied for every Element button


buttonStyle : List (Element.Attribute msg)
buttonStyle =
    [ width (px 300)
    , Background.color (rgb255 57 124 213)
    , Font.color (rgb 1 1 1)
    , paddingXY 14 10
    , Border.rounded 10
    , Font.size 20
    , Font.center
    , centerX
    ]
