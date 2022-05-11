module Pages.Levels.Level1 exposing (..)

import Assets.Data as Data exposing (..)
import Assets.GameLogic exposing (..)
import Assets.Style exposing (..)
import Browser exposing (element)
import Browser.Dom as Dom
import Browser.Events exposing (onAnimationFrameDelta)
import Char exposing (..)
import Collage exposing (..)
import Collage.Layout exposing (..)
import Collage.Render exposing (svg)
import Color exposing (..)
import Dict exposing (Dict)
import Element as Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Element.Region as Region
import Html exposing (Attribute, Html, div, h1, h3, p, pre, text, th)
import Html.Attributes exposing (autofocus, id, style, tabindex)
import Html.Events exposing (on)
import Json.Decode as Json exposing (..)
import Json.Encode as Encode exposing (..)
import Keyboard
import Keyboard.Event exposing (KeyboardEvent, decodeKeyboardEvent)
import Keyboard.Key
import List
import Round
import String exposing (..)
import Task
import Tuple3 exposing (..)



----MODEL----


amountOfJumps =
    3


winningFieldPoint =
    ( 4, 0 )


boardSizeX =
    5


boardSizeY =
    5


gameDuration =
    15


initialBoard : User -> Board
initialBoard user =
    { boardItself =
        Dict.fromList
            [ ( ( 0, 0 ), Tile )
            , ( ( 0, 1 ), Road { movement = Animated, direction = Data.Right, color = red } )
            , ( ( 0, 2 ), RoadEmpty )
            , ( ( 0, 3 ), Road { movement = Animated, direction = Data.Up, color = red } )
            , ( ( 0, 4 ), Road { movement = KeyInput, direction = Data.Up, color = blue } )
            , ( ( 1, 0 ), Tile )
            , ( ( 1, 1 ), RoadEmpty )
            , ( ( 1, 2 ), Tile )
            , ( ( 1, 3 ), Tile )
            , ( ( 1, 4 ), Tile )
            , ( ( 2, 0 ), RoadEmpty )
            , ( ( 2, 1 ), Road { movement = Animated, direction = Data.Down, color = red } )
            , ( ( 2, 2 ), RoadEmpty )
            , ( ( 2, 3 ), RoadEmpty )
            , ( ( 2, 4 ), Tile )
            , ( ( 3, 0 ), Tile )
            , ( ( 3, 1 ), Tile )
            , ( ( 3, 2 ), Tile )
            , ( ( 3, 3 ), RoadEmpty )
            , ( ( 3, 4 ), Tile )
            , ( ( 4, 0 ), RoadEmpty )
            , ( ( 4, 1 ), Road { movement = Animated, direction = Data.Down, color = red } )
            , ( ( 4, 2 ), RoadEmpty )
            , ( ( 4, 3 ), Road { movement = Animated, direction = Data.Down, color = red } )
            , ( ( 4, 4 ), RoadEmpty )
            ]
    , remainingJumps = amountOfJumps + user.extraJumps
    , winningField = winningFieldPoint
    , won = False
    }



-- Model


type Msg
    = HandleKeyboardEvent KeyboardEvent
    | NoOp
    | Tick Delta


type alias Model =
    { localUser : User
    , lastEvent : Maybe KeyboardEvent
    , time : Float
    , board : Board
    , maxTime : Float
    }


init : User -> ( Model, Cmd Msg )
init user =
    ( { localUser = user, lastEvent = Nothing, time = 0, board = initialBoard user, maxTime = gameDuration + Basics.toFloat user.extraDuration }
    , Cmd.none
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        HandleKeyboardEvent event ->
            ( { model
                | lastEvent = Just event
                , board =
                    if model.maxTime <= 0 then
                        model.board

                    else
                        moveCars (Just event) model.board
              }
            , Cmd.none
            )

        Tick dt ->
            let
                newDt =
                    -- dt + dt * Basics.toFloat model.localUser.extraGameSpeed
                    dt * Basics.toFloat (2 ^ model.localUser.extraGameSpeed)

                newTime =
                    model.time + dt

                newTimeDt =
                    model.time + newDt

                secsPassed =
                    round model.time

                newSecs =
                    round newTimeDt

                shouldUpdate =
                    secsPassed /= newSecs

                maxTimeHelper =
                    model.maxTime

                _ =
                    Debug.log "dt :" dt

                newMaxScore =
                    100 + model.board.remainingJumps * 10 - model.localUser.extraJumps * 2 - (gameDuration + model.localUser.extraDuration) + model.localUser.extraGameSpeed * 10

                newUser =
                    { extraJumps = model.localUser.extraJumps
                    , extraGameSpeed = model.localUser.extraGameSpeed
                    , extraDuration = model.localUser.extraDuration
                    , level1HS =
                        if model.localUser.level1HS < newMaxScore then
                            newMaxScore

                        else
                            model.localUser.level1HS
                    , level2HS = model.localUser.level2HS
                    , level3HS = model.localUser.level3HS
                    }
            in
            ( { model
                | board =
                    if model.board.won == True then
                        model.board

                    else if model.maxTime <= 0 then
                        model.board

                    else if shouldUpdate then
                        tick dt model.board

                    else
                        model.board
                , time = newTimeDt
                , maxTime =
                    if model.maxTime - dt > 0 then
                        model.maxTime - dt

                    else
                        0
                , localUser =
                    if model.board.won == True then
                        newUser

                    else
                        model.localUser
              }
            , Cmd.none
            )

        NoOp ->
            ( model, Cmd.none )


boardElement : Model -> Collage Msg
boardElement model =
    let
        rangeX =
            List.range 0
                (boardSizeX - 1)

        rangeY =
            List.range 0
                (boardSizeY - 1)

        oneFieldPoint ( x, y ) =
            getPointField ( x, y ) model.board

        makeTile x y =
            oneField (oneFieldPoint ( x, y )) model.board.winningField

        col x =
            vertical <|
                List.map
                    (makeTile x)
                    rangeY
    in
    horizontal <|
        List.map col rangeX


oneField : ( Data.Point, Field ) -> Data.Point -> Collage Msg
oneField ( point, field ) winner =
    let
        border =
            solid verythin <| uniform black

        ground color =
            rectangle blockSize blockSize
                |> styled ( uniform color, border )
    in
    case field of
        Road car ->
            if point == winner then
                stack [ carElement car, ground white ]

            else
                stack [ carElement car, ground roadColor ]

        RoadEmpty ->
            if point == winner then
                ground white

            else
                ground roadColor

        Tile ->
            ground tileColor

        Empty ->
            ground yellow


view : Model -> Element Msg
view model =
    let
        auticka =
            Element.html (boardElement model |> svg)
    in
    column [ Element.width fill, Element.height fill ]
        [ row
            [ Element.height fill
            , Element.width fill
            , paddingXY 10 10
            , centerX
            , spacing 40
            , Background.color (Element.rgb255 254 216 177)
            , Element.htmlAttribute
                (on "keydown" <|
                    Json.map HandleKeyboardEvent decodeKeyboardEvent
                )
            , Element.htmlAttribute (tabindex 0)
            , Element.htmlAttribute (autofocus True)
            ]
            [ column [ alignLeft, alignTop, centerX, Element.height shrink, Element.width (px 400), paddingXY 20 20, spacing 50 ]
                [ el [ alignTop, centerX, Font.size 50 ] (Element.text "Level")
                , el [ alignTop, centerX ] auticka
                , el [ alignTop, centerX ] (Element.text ("Remaning jumps: " ++ String.fromInt model.board.remainingJumps))
                , el [ alignTop, centerX ] (Element.text ("Remaning time: " ++ Round.round 1 model.maxTime))
                , if model.board.won then
                    el
                        [ alignTop
                        , centerX
                        , Font.size 50
                        , Font.bold
                        , Font.color (Element.rgb 0 1 0)
                        ]
                        (Element.text "You won!")

                  else if model.maxTime <= 0 then
                    el
                        [ alignTop
                        , centerX
                        , Font.size 50
                        , Font.bold
                        , Font.color (Element.rgb 1 0 0)
                        ]
                        (Element.text "You lost!")

                  else
                    el [] (Element.text "")
                , Element.link buttonStyle
                    { label = Element.text "Home"
                    , url = "Home"
                    }
                ]
            ]
        ]


subs : Model -> Sub Msg
subs model =
    onAnimationFrameDelta ((\dt -> dt / 1000) >> Tick)
