module Pages.HighScore exposing (..)

import Assets.Style exposing (..)
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
    = NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )



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
            , alignTop
            ]
            [ column [ alignTop, centerX, Element.height shrink, Element.width (px 400), paddingXY 20 20, spacing 50 ]
                [ el [ alignTop, centerX, Font.size 50 ] (Element.text "High Score")
                , el [ alignTop, centerX, Font.size 30 ] (Element.text ("Level 1:   " ++ String.fromInt model.localUser.level1HS))
                , el [ alignTop, centerX, Font.size 30 ] (Element.text ("Level 2:   " ++ String.fromInt model.localUser.level2HS))
                , el [ alignTop, centerX, Font.size 30 ] (Element.text ("Level 3:   " ++ String.fromInt model.localUser.level3HS))
                , el [ alignTop, centerX, Font.size 30, Font.bold ] (Element.text ("Total:   " ++ String.fromInt (model.localUser.level1HS + model.localUser.level2HS + model.localUser.level3HS)))
                , Element.link buttonStyle
                    { label = Element.text "Home"
                    , url = "Home"
                    }
                ]
            ]
        ]
