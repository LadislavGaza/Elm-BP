module Main exposing (..)

import Assets.Data as Data exposing (..)
import Assets.Style exposing (..)
import Browser exposing (Document)
import Browser.Events exposing (onClick)
import Browser.Navigation as Nav
import Collage.Layout exposing (height)
import Color
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
import Pages.Levels.Level1
import Pages.Levels.Level2
import Pages.Levels.Level3
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
    | HighScore Pages.HighScore.Model
    | Level1 Pages.Levels.Level1.Model
    | Level2 Pages.Levels.Level2.Model
    | Level3 Pages.Levels.Level3.Model
    | Tutorial Pages.Tutorial.Model
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
    , extraJumps = 0
    , extraGameSpeed = 0
    , extraDuration = 0
    , level1HS = 0
    , level2HS = 0
    , level3HS = 0
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
    | HighScoreMsg Pages.HighScore.Msg
    | Level1Msg Pages.Levels.Level1.Msg
    | Level2Msg Pages.Levels.Level2.Msg
    | Level3Msg Pages.Levels.Level3.Msg
    | TutorialMsg Pages.Tutorial.Msg
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

        HighScoreMsg msg ->
            case model.page of
                HighScore highScore ->
                    stepHighScore model (Pages.HighScore.update msg highScore)

                _ ->
                    ( model, Cmd.none )

        SettingsMsg msg ->
            case model.page of
                Settings settings ->
                    -- stepSettings model (Pages.Settings.update msg settings)
                    let
                        ( newChildModel, newChildCmd ) =
                            Pages.Settings.update msg settings

                        ( newModel, newCmd ) =
                            stepSettings model ( newChildModel, newChildCmd )

                        newUser : User
                        newUser =
                            { username = newChildModel.localUser.username
                            , extraJumps = newChildModel.localUser.extraJumps
                            , extraGameSpeed = newChildModel.localUser.extraGameSpeed
                            , extraDuration = newChildModel.localUser.extraDuration
                            , level1HS = newChildModel.localUser.level1HS
                            , level2HS = newChildModel.localUser.level2HS
                            , level3HS = newChildModel.localUser.level3HS
                            }
                    in
                    ( { newModel | user = newUser }, Cmd.batch [ newCmd, encodeUser newUser ] )

                _ ->
                    ( model, Cmd.none )

        Level1Msg msg ->
            case model.page of
                Level1 level1 ->
                    let
                        ( newChildModel, newChildCmd ) =
                            Pages.Levels.Level1.update msg level1

                        ( newModel, newCmd ) =
                            stepLevel1 model ( newChildModel, newChildCmd )

                        newUser : User
                        newUser =
                            { username = newChildModel.localUser.username
                            , extraJumps = newChildModel.localUser.extraJumps
                            , extraGameSpeed = newChildModel.localUser.extraGameSpeed
                            , extraDuration = newChildModel.localUser.extraDuration
                            , level1HS = newChildModel.localUser.level1HS
                            , level2HS = newChildModel.localUser.level2HS
                            , level3HS = newChildModel.localUser.level3HS
                            }
                    in
                    ( { newModel | user = newUser }, Cmd.batch [ newCmd, encodeUser newUser ] )

                _ ->
                    ( model, Cmd.none )

        Level2Msg msg ->
            case model.page of
                Level2 level2 ->
                    let
                        ( newChildModel, newChildCmd ) =
                            Pages.Levels.Level2.update msg level2

                        ( newModel, newCmd ) =
                            stepLevel2 model ( newChildModel, newChildCmd )

                        newUser : User
                        newUser =
                            { username = newChildModel.localUser.username
                            , extraJumps = newChildModel.localUser.extraJumps
                            , extraGameSpeed = newChildModel.localUser.extraGameSpeed
                            , extraDuration = newChildModel.localUser.extraDuration
                            , level1HS = newChildModel.localUser.level1HS
                            , level2HS = newChildModel.localUser.level2HS
                            , level3HS = newChildModel.localUser.level3HS
                            }
                    in
                    ( { newModel | user = newUser }, Cmd.batch [ newCmd, encodeUser newUser ] )

                _ ->
                    ( model, Cmd.none )

        Level3Msg msg ->
            case model.page of
                Level3 level3 ->
                    let
                        ( newChildModel, newChildCmd ) =
                            Pages.Levels.Level3.update msg level3

                        ( newModel, newCmd ) =
                            stepLevel3 model ( newChildModel, newChildCmd )

                        newUser : User
                        newUser =
                            { username = newChildModel.localUser.username
                            , extraJumps = newChildModel.localUser.extraJumps
                            , extraGameSpeed = newChildModel.localUser.extraGameSpeed
                            , extraDuration = newChildModel.localUser.extraDuration
                            , level1HS = newChildModel.localUser.level1HS
                            , level2HS = newChildModel.localUser.level2HS
                            , level3HS = newChildModel.localUser.level3HS
                            }
                    in
                    ( { newModel | user = newUser }, Cmd.batch [ newCmd, encodeUser newUser ] )

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
                , route (s "HighScore") (stepHighScore model (Pages.HighScore.init model.user))
                , route (s "Settings") (stepSettings model (Pages.Settings.init model.user))
                , route (s "Level1") (stepLevel1 model (Pages.Levels.Level1.init model.user))
                , route (s "Level2") (stepLevel2 model (Pages.Levels.Level2.init model.user))
                , route (s "Level3") (stepLevel3 model (Pages.Levels.Level3.init model.user))
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


stepLevel1 : Model -> ( Pages.Levels.Level1.Model, Cmd Pages.Levels.Level1.Msg ) -> ( Model, Cmd Msg )
stepLevel1 model ( level1, cmds ) =
    ( { model | page = Level1 level1 }
    , Cmd.map Level1Msg cmds
    )


stepLevel2 : Model -> ( Pages.Levels.Level2.Model, Cmd Pages.Levels.Level2.Msg ) -> ( Model, Cmd Msg )
stepLevel2 model ( level2, cmds ) =
    ( { model | page = Level2 level2 }
    , Cmd.map Level2Msg cmds
    )


stepLevel3 : Model -> ( Pages.Levels.Level3.Model, Cmd Pages.Levels.Level3.Msg ) -> ( Model, Cmd Msg )
stepLevel3 model ( level3, cmds ) =
    ( { model | page = Level3 level3 }
    , Cmd.map Level3Msg cmds
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

            HighScore highScoreModel ->
                Sub.none

            Settings settingsModel ->
                Sub.none

            Level1 level1Model ->
                Sub.map Level1Msg (Pages.Levels.Level1.subs level1Model)

            Level2 level2Model ->
                Sub.map Level2Msg (Pages.Levels.Level2.subs level2Model)

            Level3 level3Model ->
                Sub.map Level3Msg (Pages.Levels.Level3.subs level3Model)

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

        Home homeModel ->
            Element.map HomeMsg (Pages.Home.view homeModel)

        HighScore highScoreModel ->
            Element.map HighScoreMsg (Pages.HighScore.view highScoreModel)

        Settings settingsModel ->
            Element.map SettingsMsg (Pages.Settings.view settingsModel)

        Level1 level1Model ->
            Element.map Level1Msg (Pages.Levels.Level1.view level1Model)

        Level2 level2Model ->
            Element.map Level2Msg (Pages.Levels.Level2.view level2Model)

        Level3 level3Model ->
            Element.map Level3Msg (Pages.Levels.Level3.view level3Model)

        Tutorial tutorialModel ->
            Element.map TutorialMsg (Pages.Tutorial.view tutorialModel)

        Global ->
            globalHomeView


globalHomeView : Element Msg
globalHomeView =
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



----PORTS----
--


encodeUser : User -> Cmd msg
encodeUser user =
    let
        json =
            Encode.object
                [ ( "username", Encode.string user.username )
                , ( "extraJumps", Encode.int user.extraJumps )
                , ( "extraGameSpeed", Encode.int user.extraGameSpeed )
                , ( "extraDuration", Encode.int user.extraDuration )
                , ( "level1HS", Encode.int user.level1HS )
                , ( "level2HS", Encode.int user.level2HS )
                , ( "level3HS", Encode.int user.level3HS )
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
