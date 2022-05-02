module Pages.Home exposing (..)

import Browser
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Region as Region
import Html exposing (Html, button, div, h1, img, text)
import Html.Attributes exposing (src, style, type_)



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
    -- Element.html helperView
    column [ width fill, Element.height fill ]
        [ row [ Element.height fill, width fill, paddingXY 10 10, centerX, spacing 30, Background.color (rgb255 254 216 177) ]
            [ column [ alignLeft, alignTop, centerX, Element.height shrink, width (px 400), paddingXY 20 20, spacing 15 ]
                [ Element.image [ alignTop, centerX, Element.height (px 150), width (px 150) ] { src = "/logo.svg", description = "nah" }
                , el [ alignTop, centerX, Font.size 50 ] (Element.text "Menu")
                , Element.link buttonStyle
                    { label = Element.text "Level 1"
                    , url = "Level"
                    }
                , Element.link buttonStyle
                    { label = Element.text "Level 2"
                    , url = "Editor"
                    }
                , Element.link buttonStyle
                    { label = Element.text "Settings"
                    , url = "Settings"
                    }
                , Element.link buttonStyle
                    { label = Element.text "High Score"
                    , url = "HighScore"
                    }
                , Element.link buttonStyle
                    { label = Element.text "Editor"
                    , url = "Editor"
                    }
                , Element.link buttonStyle
                    { label = Element.text "Log In"
                    , url = "Ano"
                    }
                ]
            ]
        ]


buttonStyle : List (Element.Attribute msg)
buttonStyle =
    [ width (px 300)
    , Background.color (rgb255 57 124 213)
    , Font.color (rgb 1 1 1)
    , paddingXY 14 10

    -- , style "margin-top" "10px"
    -- , style "margin-left" "10px"
    , Border.rounded 10
    , Font.size 20
    , Font.center
    , centerX
    ]



-- helperView : Html Msg
-- helperView =
--     div []
--         [ img [ src "/logo.svg" ] []
--         , h1 [] [ Html.text "Jumpero" ]
--
--         -- , div buttonStyle [ text "asdasd" ]
--         , div []
--             [ button buttonStyle
--                 [ Html.text "Úrovne" ]
--             ]
--         , div []
--             [ button ([ type_ "submit" ] ++ buttonStyle)
--                 [ Html.text "Nastavenia" ]
--             ]
--         , div []
--             [ button ([ type_ "submit" ] ++ buttonStyle)
--                 [ Html.text "Najvyššie skóre" ]
--             ]
--         , div []
--             [ button ([ type_ "submit" ] ++ buttonStyle)
--                 [ Html.text "Editor mapy" ]
--             ]
--         , div []
--             [ button ([ type_ "submit" ] ++ buttonStyle)
--                 [ Html.text "Odhlásenie" ]
--             ]
--         ]
--
--
-- buttonStyle : List (Html.Attribute msg)
-- buttonStyle =
--     [ style "width" "300px"
--     , style "background-color" "#397cd5"
--     , style "color" "white"
--     , style "padding" "14px 20px"
--     , style "margin-top" "10px"
--     , style "margin-left" "10px"
--     , style "border-radius" "4px"
--     , style "font-size" "24px"
--     , style "text-align" "center"
--     ]
-- view : Model -> Element Msg
-- view model =
--     column [ width fill, height fill ]
--         [ row [ height fill, width fill, paddingXY 10 10, centerX, spacing 40 ]
--             [ column [ alignLeft, alignTop, centerX, height fill, width (px 400), paddingXY 20 20, Border.rounded 15, spacing 30, Border.width 2 ]
--                 [ Element.image [ alignTop, centerX, height (px 50), width (px 50) ] { src = "/elm.png", description = "nah" }
--                 , el [ alignTop, centerX, Font.size 50 ] (text "Menu")
--                 , Element.link []
--                     { label = text "PLAY"
--                     , url = "Editor"
--                     }
--                 , Element.link []
--                     { label = text "TUTORIAL"
--                     , url = "Guide"
--                     }
--                 , Element.link []
--                     { label = text "INFO"
--                     , url = "Info"
--                     }
--                 ]
--             ]
--         ]
