module Pages.Settings exposing (..)

import Browser exposing (element)
import Browser.Dom as Dom
import Browser.Events exposing (onAnimationFrameDelta)
import Char exposing (..)
import Collage exposing (..)
import Collage.Layout exposing (..)
import Collage.Render exposing (svg)
import Color exposing (..)
import Data exposing (..)
import Dict exposing (Dict)
import Dict.Extra as Extra
import Element as Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Element.Region as Region
import Html exposing (Attribute, Html, div, h1, h3, p, pre, text)
import Html.Attributes exposing (autofocus, id, style, tabindex)
import Html.Events exposing (on)
import Json.Decode as Json exposing (..)
import Json.Encode as Encode exposing (..)
import Keyboard
import Keyboard.Event exposing (KeyboardEvent, decodeKeyboardEvent)
import Keyboard.Key
import List
import String exposing (..)
import Task



----MODEL----


type alias Model =
    { localUser : User
    }


init : User -> ( Model, Cmd Msg )
init user =
    ( { localUser = user }
    , Cmd.none
    )



---UPDATE----


type Msg
    = Jumps Int
    | UpdateUser User


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Jumps jumps ->
            let
                newUser =
                    { username = model.localUser.username
                    , extraJumps = jumps
                    , extraGameSpeed = model.localUser.extraGameSpeed
                    , extraDuration = model.localUser.extraDuration
                    , level1HS = model.localUser.level1HS
                    , level2HS = model.localUser.level2HS
                    , level3HS = model.localUser.level3HS
                    }
            in
            ( { model | localUser = newUser }, Cmd.none )

        UpdateUser user ->
            ( { model | localUser = user }, Cmd.none )



---- VIEW ----


view : Model -> Element Msg
view model =
    column [ Element.width fill, Element.height fill ]
        [ row
            [ Element.height fill
            , Element.width fill
            , paddingXY 10 10
            , centerX
            , spacing 40
            , Background.color (Element.rgb255 254 216 177)
            ]
            [ column [ alignLeft, alignTop, centerX, Element.height fill, Element.width (px 400), paddingXY 20 20, spacing 15 ]
                [ el [ alignTop, centerX, Font.size 50 ] (Element.text "Level")
                , sliderJumps model
                , Element.link buttonStyle
                    { label = Element.text "Home"
                    , url = "Home"
                    }
                ]
            ]
        ]


buttonStyle : List (Element.Attribute msg)
buttonStyle =
    [ Element.width (px 300)
    , Background.color (Element.rgb255 57 124 213)
    , Font.color (Element.rgb 1 1 1)
    , paddingXY 14 10
    , Border.rounded 10
    , Font.size 20
    , Font.center
    , centerX
    ]


sliderJumps : Model -> Element Msg
sliderJumps model =
    Input.slider
        [ centerY
        , centerX
        , alignTop
        , Element.behindContent
            (Element.el
                [ Element.width Element.fill
                , Element.height (Element.px 2)
                , Element.centerY
                , Background.color (Element.rgb255 57 124 213)
                , Border.rounded 2
                ]
                Element.none
            )
        ]
        { onChange = round >> Jumps
        , label =
            Input.labelAbove []
                (Element.text ("Extra jumps: " ++ String.fromInt model.localUser.extraJumps))
        , min = 1
        , max = 10
        , step = Just 1
        , value = Basics.toFloat model.localUser.extraJumps
        , thumb =
            Input.defaultThumb
        }
