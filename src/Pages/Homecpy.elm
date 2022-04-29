module Pages.Home exposing (..)

-- import Element exposing (..)
-- import Element.Background as Background
-- import Element.Border as Border
-- import Element.Font as Font
-- import Element.Region as Region

import Browser
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


view : Model -> Html Msg
view model =
    div []
        [ img [ src "/logo.svg" ] []
        , h1 [] [ text "Jumpero" ]

        -- , div buttonStyle [ text "asdasd" ]
        , div []
            [ button buttonStyle
                [ text "Úrovne" ]
            ]
        , div []
            [ button ([ type_ "submit" ] ++ buttonStyle)
                [ text "Nastavenia" ]
            ]
        , div []
            [ button ([ type_ "submit" ] ++ buttonStyle)
                [ text "Najvyššie skóre" ]
            ]
        , div []
            [ button ([ type_ "submit" ] ++ buttonStyle)
                [ text "Editor mapy" ]
            ]
        , div []
            [ button ([ type_ "submit" ] ++ buttonStyle)
                [ text "Odhlásenie" ]
            ]
        ]


buttonStyle : List (Html.Attribute msg)
buttonStyle =
    [ style "width" "300px"
    , style "background-color" "#397cd5"
    , style "color" "white"
    , style "padding" "14px 20px"
    , style "margin-top" "10px"
    , style "margin-left" "10px"
    , style "border-radius" "4px"
    , style "font-size" "24px"
    , style "text-align" "center"
    ]
