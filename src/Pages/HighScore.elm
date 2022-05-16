module Pages.HighScore exposing (..)

import Assets.Data exposing (..)
import Assets.Style exposing (..)
import Element exposing (..)
import Element.Background as Background
import Element.Font as Font
import String



-- MODEL


type alias Model =
    { localUser : User
    }


init : User -> ( Model, Cmd Msg )
init user =
    ( { localUser = user }
    , Cmd.none
    )



-- UPDATE


type Msg
    = NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )



-- VIEW


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
