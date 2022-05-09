module Main exposing (..)

import Browser exposing (Document)
import Browser.Events exposing (onClick)
import Browser.Navigation as Nav
import Collage.Layout exposing (height)
import Color
import Data exposing (..)
import Element as Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Html as Html exposing (Html, a, button, div, h1, img, text)
import Html.Attributes exposing (href, src, style, type_)
import Html.Events exposing (onClick)
import Json.Decode as Decode
import Json.Encode as Encode
import Pages.Editor
import Pages.HighScore
import Pages.Home
import Pages.Level1
import Pages.Level2
import Pages.Login
import Pages.Settings
import Pages.Tutorial
import Ports
import Time
import Url
import Url.Parser as Parser exposing ((</>), Parser, custom, fragment, map, oneOf, s, top)



---- MODEL ----


type Page
    = Home Pages.Home.Model
    | Editor Pages.Editor.Model
    | HighScore Pages.HighScore.Model
    | Level1 Pages.Level1.Model
    | Level2 Pages.Level2.Model
    | Tutorial Pages.Tutorial.Model
      -- | Login Pages.Login.Model
    | Settings Pages.Settings.Model
    | Global
    | NotFound


type alias Model =
    { key : Nav.Key
    , url : Url.Url
    , page : Page
    , user : User
    }


unknownUser : User
unknownUser =
    { username = "Player"
    , level = 1
    }


init : Maybe User -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init maybeUser url key =
    let
        user =
            case maybeUser of
                Nothing ->
                    unknownUser

                Just u ->
                    u
    in
    ( Model key url Global user, Ports.play (Encode.bool True) )



---- UPDATE ----


type Msg
    = LinkClicked Browser.UrlRequest
    | UrlChanged Url.Url
    | HomeMsg Pages.Home.Msg
    | EditorMsg Pages.Editor.Msg
    | HighScoreMsg Pages.HighScore.Msg
    | Level1Msg Pages.Level1.Msg
    | Level2Msg Pages.Level2.Msg
    | TutorialMsg Pages.Tutorial.Msg
      -- | LoginMsg Pages.Login.Msg
    | SettingsMsg Pages.Settings.Msg
    | PlayMe Time.Posix


update : Msg -> Model -> ( Model, Cmd Msg )
update message model =
    case message of
        LinkClicked urlRequest ->
            case urlRequest of
                Browser.Internal url ->
                    ( model, Nav.pushUrl model.key (Url.toString url) )

                Browser.External href ->
                    ( model, Nav.load href )

        UrlChanged url ->
            stepUrl url
                model

        HomeMsg msg ->
            case model.page of
                Home home ->
                    stepHome model (Pages.Home.update msg home)

                _ ->
                    ( model, Cmd.none )

        EditorMsg msg ->
            case model.page of
                Editor editor ->
                    stepEditor model (Pages.Editor.update msg editor)

                _ ->
                    ( model, Cmd.none )

        HighScoreMsg msg ->
            case model.page of
                HighScore highScore ->
                    stepHighScore model (Pages.HighScore.update msg highScore)

                _ ->
                    ( model, Cmd.none )

        SettingsMsg msg ->
            case model.page of
                Settings settings ->
                    stepSettings model (Pages.Settings.update msg settings)

                _ ->
                    ( model, Cmd.none )

        --
        -- LoginMsg msg ->
        --     case model.page of
        --         Login login ->
        --             stepLogin model (Pages.Login.update msg login)
        --
        --         _ ->
        --             ( model, Cmd.none )
        --
        Level1Msg msg ->
            case model.page of
                Level1 level1 ->
                    stepLevel1 model (Pages.Level1.update msg level1)

                _ ->
                    ( model, Cmd.none )

        Level2Msg msg ->
            case model.page of
                Level2 level2 ->
                    stepLevel2 model (Pages.Level2.update msg level2)

                _ ->
                    ( model, Cmd.none )

        TutorialMsg msg ->
            case model.page of
                Tutorial tutorial ->
                    stepTutorial model (Pages.Tutorial.update msg tutorial)

                _ ->
                    ( model, Cmd.none )

        PlayMe _ ->
            ( model, Ports.play (Encode.bool True) )


stepUrl : Url.Url -> Model -> ( Model, Cmd Msg )
stepUrl url model =
    let
        parser =
            oneOf
                [ route (s "Home") (stepHome model (Pages.Home.init ()))
                , route (s "Editor") (stepEditor model (Pages.Editor.init ()))
                , route (s "HighScore") (stepHighScore model (Pages.HighScore.init model.user))
                , route (s "Settings") (stepSettings model (Pages.Settings.init model.user))

                -- , route (s "Login") (stepLogin model (Pages.Login.init model.user))
                , route (s "Level1") (stepLevel1 model (Pages.Level1.init model.user))
                , route (s "Level2") (stepLevel2 model (Pages.Level2.init model.user))
                , route (s "Tutorial") (stepTutorial model (Pages.Tutorial.init model.user))
                ]
    in
    case Parser.parse parser url of
        Just answer ->
            answer

        Nothing ->
            ( { model | page = NotFound }, Cmd.none )


stepHome : Model -> ( Pages.Home.Model, Cmd Pages.Home.Msg ) -> ( Model, Cmd Msg )
stepHome model ( home, cmds ) =
    ( { model | page = Home home }
    , Cmd.map HomeMsg cmds
    )


stepEditor : Model -> ( Pages.Editor.Model, Cmd Pages.Editor.Msg ) -> ( Model, Cmd Msg )
stepEditor model ( editor, cmds ) =
    ( { model | page = Editor editor }
    , Cmd.map EditorMsg cmds
    )


stepHighScore : Model -> ( Pages.HighScore.Model, Cmd Pages.HighScore.Msg ) -> ( Model, Cmd Msg )
stepHighScore model ( highScore, cmds ) =
    ( { model | page = HighScore highScore }
    , Cmd.map HighScoreMsg cmds
    )


stepSettings : Model -> ( Pages.Settings.Model, Cmd Pages.Settings.Msg ) -> ( Model, Cmd Msg )
stepSettings model ( settings, cmds ) =
    ( { model | page = Settings settings }
    , Cmd.map SettingsMsg cmds
    )



--
--
-- stepLogin : Model -> ( Pages.Login.Model, Cmd Pages.Login.Msg ) -> ( Model, Cmd Msg )
-- stepLogin model ( login, cmds ) =
--     ( { model | page = Login login }
--     , Cmd.map LoginMsg cmds
--     )
--
--


stepLevel1 : Model -> ( Pages.Level1.Model, Cmd Pages.Level1.Msg ) -> ( Model, Cmd Msg )
stepLevel1 model ( level1, cmds ) =
    ( { model | page = Level1 level1 }
    , Cmd.map Level1Msg cmds
    )


stepLevel2 : Model -> ( Pages.Level2.Model, Cmd Pages.Level2.Msg ) -> ( Model, Cmd Msg )
stepLevel2 model ( level2, cmds ) =
    ( { model | page = Level2 level2 }
    , Cmd.map Level2Msg cmds
    )


stepTutorial : Model -> ( Pages.Tutorial.Model, Cmd Pages.Tutorial.Msg ) -> ( Model, Cmd Msg )
stepTutorial model ( tutorial, cmds ) =
    ( { model | page = Tutorial tutorial }
    , Cmd.map TutorialMsg cmds
    )



{- https://github.com/elm/package.elm-lang.org/blob/master/src/frontend/Main.elm -}


route : Parser a b -> a -> Parser (b -> c) c
route parser handler =
    Parser.map handler parser



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Time.every 5000 PlayMe
        , case model.page of
            NotFound ->
                Sub.none

            Home homeModel ->
                Sub.none

            Editor editorModel ->
                Sub.map EditorMsg (Pages.Editor.subs editorModel)

            HighScore highScoreModel ->
                Sub.none

            Settings settingsModel ->
                Sub.map SettingsMsg (Pages.Settings.subs settingsModel)

            --
            -- Login loginModel ->
            --     Sub.none
            --
            Level1 level1Model ->
                Sub.map Level1Msg (Pages.Level1.subs level1Model)

            Level2 level2Model ->
                Sub.map Level2Msg (Pages.Level2.subs level2Model)

            Tutorial tutorialModel ->
                Sub.map TutorialMsg (Pages.Tutorial.subs tutorialModel)

            Global ->
                Sub.none
        ]



---- VIEW ----


view : Model -> Browser.Document Msg
view model =
    { title = "Jumpero"
    , body = [ Element.layout [] (searchView model) ]
    }


searchView : Model -> Element Msg
searchView model =
    case model.page of
        NotFound ->
            globalHomeView

        --
        Home homeModel ->
            Element.map HomeMsg (Pages.Home.view homeModel)

        --
        Editor editorModel ->
            Element.map EditorMsg (Pages.Editor.view editorModel)

        --
        HighScore highScoreModel ->
            Element.map HighScoreMsg (Pages.HighScore.view highScoreModel)

        Settings settingsModel ->
            Element.map SettingsMsg (Pages.Settings.view settingsModel)

        --
        -- Login loginModel ->
        --     Html.map LoginMsg (Pages.Login.view loginModel)
        Level1 level1Model ->
            Element.map Level1Msg (Pages.Level1.view level1Model)

        Level2 level2Model ->
            Element.map Level2Msg (Pages.Level2.view level2Model)

        Tutorial tutorialModel ->
            Element.map TutorialMsg (Pages.Tutorial.view tutorialModel)

        Global ->
            globalHomeView


globalHomeView : Element Msg
globalHomeView =
    column [ width fill, Element.height fill ]
        [ row [ Element.height fill, width fill, paddingXY 10 10, centerX, spacing 30, Background.color (rgb255 254 216 177) ]
            [ column [ alignLeft, alignTop, centerX, Element.height shrink, width (px 400), paddingXY 20 20, spacing 15 ]
                [ Element.image [ alignTop, centerX, Element.height (px 150), width (px 150) ] { src = "/logo.svg", description = "nah" }
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



-- globalHomeView : Element Msg
-- globalHomeView =
--     Element.html helperView
--
--
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



----PORTS----
--


encodeUser : User -> Cmd msg
encodeUser user =
    let
        json =
            Encode.object
                [ ( "username", Encode.string user.username )
                , ( "level", Encode.int user.level )
                ]
    in
    Ports.storeUser json



---- PROGRAM ----


main : Program (Maybe User) Model Msg
main =
    Browser.application
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        , onUrlChange = UrlChanged
        , onUrlRequest = LinkClicked
        }
