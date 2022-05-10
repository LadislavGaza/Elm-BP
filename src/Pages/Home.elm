module Pages.Home exposing (..)

import Assets.Style exposing (..)
import Browser
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Region as Region
import Html exposing (Html, button, div, h1, img, text)
import Html.Attributes exposing (autofocus, src, style, type_)



----MODEL----


type alias Model =
    {}


init : () -> ( Model, Cmd Msg )
init _ =
    ( {}, Cmd.none )



---UPDATE----


type Msg
    = None


update : Msg -> Model -> ( Model, Cmd Msg )
update message model =
    case message of
        None ->
            ( model, Cmd.none )



---- VIEW ----


view : Model -> Element Msg
view model =
    column [ width fill, Element.height fill ]
        [ row [ Element.height fill, width fill, paddingXY 10 10, centerX, spacing 30, Background.color (rgb255 254 216 177) ]
            [ column [ alignLeft, alignTop, centerX, Element.height shrink, width (px 400), paddingXY 20 20, spacing 25 ]
                [ Element.image [ alignTop, centerX, Element.height (px 150), width (px 250), paddingEach { top = 0, right = 0, bottom = 0, left = 20 } ] { src = "/logo.svg", description = "nah" }
                , el [ alignTop, centerX, Font.size 50 ] (Element.text "Menu")
                , Element.link buttonStyle
                    { label = Element.text "Tutorial"
                    , url = "Tutorial"
                    }
                , Element.link buttonStyle
                    { label = Element.text "Level 1"
                    , url = "Level1"
                    }
                , Element.link buttonStyle
                    { label = Element.text "Level 2"
                    , url = "Level2"
                    }
                , Element.link buttonStyle
                    { label = Element.text "Level 3"
                    , url = "Level3"
                    }
                , Element.link buttonStyle
                    { label = Element.text "Settings"
                    , url = "Settings"
                    }
                , Element.link buttonStyle
                    { label = Element.text "High Score"
                    , url = "HighScore"
                    }
                ]
            ]
        ]
