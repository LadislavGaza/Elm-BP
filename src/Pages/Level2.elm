module Pages.Level2 exposing (..)

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
import Data exposing (..)
import Debug exposing (toString)
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
import String exposing (..)
import Task
import Tuple3 exposing (..)



----MODEL----


amountOfJumps =
    5


winningFieldPoint =
    ( 3, 3 )


boardSizeX =
    5


boardSizeY =
    4


initialBoard : Board
initialBoard =
    { boardItself =
        Dict.fromList
            [ ( ( 0, 0 ), Road { movement = KeyInput, direction = Data.Right, color = green } )
            , ( ( 0, 1 ), Tile )
            , ( ( 0, 2 ), Tile )
            , ( ( 0, 3 ), Tile )
            , ( ( 1, 0 ), RoadEmpty )
            , ( ( 1, 1 ), RoadEmpty )
            , ( ( 1, 2 ), RoadEmpty )
            , ( ( 1, 3 ), Road { movement = Animated, direction = Data.Right, color = red } )
            , ( ( 2, 0 ), Tile )
            , ( ( 2, 1 ), Tile )
            , ( ( 2, 2 ), RoadEmpty )
            , ( ( 2, 3 ), Tile )
            , ( ( 3, 0 ), Tile )
            , ( ( 3, 1 ), Tile )
            , ( ( 3, 2 ), Road { movement = Animated, direction = Data.Left, color = red } )
            , ( ( 3, 3 ), RoadEmpty )
            , ( ( 4, 0 ), RoadEmpty )
            , ( ( 4, 1 ), RoadEmpty )
            , ( ( 4, 2 ), RoadEmpty )
            , ( ( 4, 3 ), RoadEmpty )
            ]
    , remainingJumps = amountOfJumps
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
    ( { localUser = user, lastEvent = Nothing, time = 0, board = initialBoard, maxTime = 10 }
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
                newTime =
                    model.time + dt

                secsPassed =
                    round model.time

                newSecs =
                    round newTime

                shouldUpdate =
                    secsPassed /= newSecs

                maxTimeHelper =
                    model.maxTime

                newMaxScore =
                    10 + model.board.remainingJumps * 10

                _ =
                    Debug.log "maxTime :" model.maxTime

                newUser =
                    { username = model.localUser.username
                    , extraJumps = model.localUser.extraJumps
                    , extraGameSpeed = model.localUser.extraGameSpeed
                    , extraDuration = model.localUser.extraDuration
                    , level1HS = model.localUser.level1HS
                    , level2HS =
                        if model.localUser.level2HS < newMaxScore then
                            newMaxScore

                        else
                            model.localUser.level2HS
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
                , time = newTime
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
                , el [ alignTop, centerX ] (Element.text ("Remaning jumps:" ++ toString model.board.remainingJumps))
                , el [ alignTop, centerX ] (Element.text ("Remaning time:" ++ toString (round model.maxTime)))
                , if model.board.won then
                    el
                        [ alignTop
                        , centerX
                        , Font.size 50
                        , Font.bold
                        , Font.color (Element.rgb 0 1 0)
                        ]
                        (Element.text "Vyhral si!")

                  else if model.maxTime <= 0 then
                    el
                        [ alignTop
                        , centerX
                        , Font.size 50
                        , Font.bold
                        , Font.color (Element.rgb 1 0 0)
                        ]
                        (Element.text "Prehral si!")

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
subs _ =
    onAnimationFrameDelta ((\dt -> dt / 1000) >> Tick)
