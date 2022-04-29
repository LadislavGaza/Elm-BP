module Main exposing (..)

import Browser
import Browser.Navigation as Nav
import Home
import Html exposing (Html, button, div, h1, img, text)
import Html.Attributes exposing (src, style, type_)
import Url
import Url.Parser as Parser exposing ((</>), Parser, custom, fragment, map, oneOf, s, top)



---- MODEL ----


type Page
    = Home Home.Model
    | NotFound


type alias Model =
    { key : Nav.Key
    , url : Url.Url
    , page : Page
    }


init : Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init maybeUser url key =
    ( Model key url, Ports.play (Encode.bool True) )



---- UPDATE ----


type Msg
    = LinkClicked Browser.UrlRequest
    | UrlChanged Url.Url
    | HomeMsg Home.Msg
    | NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        LinkClicked urlRequest ->
            case urlRequest of
                Browser.Internal url ->
                    ( model, Nav.pushUrl model.key (Url.toString url) )

                Browser.External href ->
                    ( model, Nav.load href )

        UrlChanged url ->
            stepUrl url model


stepUrl : Url.Url -> Model -> ( Model, Cmd Msg )
stepUrl url model =
    let
        parser =
            oneOf
                [ route (s "Home") (stepHome model (Home.init ()))
                ]
    in
    case Parser.parse parser url of
        Just answer ->
            answer

        Nothing ->
            ( { model | page = NotFound }, Cmd.none )


stepHome : Model -> ( Home.Model, Cmd Home.Msg ) -> ( Model, Cmd Msg )
stepHome model ( home, cmds ) =
    ( { model | page = Home home }
    , Cmd.map HomeMsg cmds
    )


route : Parser a b -> a -> Parser (b -> c) c
route parser handler =
    Parser.map handler parser



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
        ]



---- PROGRAM ----


main : Program () Model Msg
main =
    Browser.application
        { init = \_ -> init
        , onUrlChange = UrlChanged
        , onUrlRequest = LinkClicked
        , subscriptions = always Sub.none
        , update = update
        , view = view
        }


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
