module Pages.Levels.Tutorial exposing (..)

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
import Tuple3 exposing (..)



----MODEL----


amountOfJumps =
    10


winningFieldPoint =
    ( 0, 3 )


boardSizeX =
    4


boardSizeY =
    4


gameDuration =
    90


initialBoard : User -> Board
initialBoard user =
    { boardItself =
        Dict.fromList
            [ ( ( 0, 0 ), Tile )
            , ( ( 0, 1 ), Road { movement = KeyInput, direction = Data.Right, color = blue } )
            , ( ( 0, 2 ), Tile )
            , ( ( 0, 3 ), RoadEmpty )
            , ( ( 1, 0 ), Tile )
            , ( ( 1, 1 ), RoadEmpty )
            , ( ( 1, 2 ), Tile )
            , ( ( 1, 3 ), RoadEmpty )
            , ( ( 2, 0 ), Road { movement = Animated, direction = Data.Down, color = red } )
            , ( ( 2, 1 ), RoadEmpty )
            , ( ( 2, 2 ), RoadEmpty )
            , ( ( 2, 3 ), Road { movement = Animated, direction = Data.Down, color = red } )
            , ( ( 3, 0 ), Tile )
            , ( ( 3, 1 ), Road { movement = Animated, direction = Data.Right, color = red } )
            , ( ( 3, 2 ), Tile )
            , ( ( 3, 3 ), Tile )
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
    Element.row [ padding 10, spacing 7, alignTop, centerX, Background.color (Element.rgb255 254 216 177), Element.width fill, Element.height fill ]
        [ Element.row
            [ alignTop
            , centerX
            , Element.htmlAttribute
                (on "keydown" <|
                    Json.map HandleKeyboardEvent decodeKeyboardEvent
                )
            , Element.htmlAttribute (tabindex 0)
            , Element.htmlAttribute (autofocus True)
            ]
            [ column [ alignTop, alignTop, centerX, Element.height shrink, Element.width (px 700), paddingXY 20 20, spacing 50 ]
                [ el [ alignTop, centerX, Font.size 50 ] (Element.text "Tutorial")
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
        , Element.row [ alignTop, centerX ]
            [ column [ alignLeft, alignTop, centerX, Element.height shrink, Element.width (px 600), paddingXY 20 20, spacing 40 ]
                [ el [ alignTop, centerX, Font.size 45 ] (Element.text "Explanation")
                , paragraph []
                    [ el [ alignTop, centerX, Font.size 25 ] (Element.text "The goal of the game is to get into the finish tile of the board displayed with ")
                    , el
                        [ alignTop
                        , centerX
                        , Font.size 25
                        , Font.color (Element.rgb 1 1 1)
                        , Font.shadow
                            { offset = ( 0, 0 )
                            , blur = 10
                            , color = Element.rgb 0 0 0
                            }
                        ]
                        (Element.text "white ")
                    , el [ alignTop, centerX, Font.size 25 ] (Element.text "color.")
                    ]
                , paragraph []
                    [ el
                        [ alignTop
                        , centerX
                        , Font.size 25
                        , Font.color (Element.rgb255 138 226 52)
                        , Font.shadow
                            { offset = ( 0, 0 )
                            , blur = 5
                            , color = Element.rgb 0 0 0
                            }
                        ]
                        (Element.text "Green ")
                    , el [ alignTop, centerX, Font.size 25 ] (Element.text "tiles are forest that you cannot enter. You can only move on a road marked as ")
                    , el
                        [ alignTop
                        , centerX
                        , Font.size 25
                        , Font.color (Element.rgb255 186 189 182)
                        , Font.shadow
                            { offset = ( 0, 0 )
                            , blur = 5
                            , color = Element.rgb 0 0 0
                            }
                        ]
                        (Element.text "gray ")
                    , el [ alignTop, centerX, Font.size 25 ] (Element.text "tiles")
                    ]
                , paragraph []
                    [ el [ alignTop, centerX, Font.size 25 ] (Element.text "You control the blue car by arrows on your keyboard →↑↓←.")
                    ]
                , paragraph []
                    [ el [ alignTop, centerX, Font.size 25 ] (Element.text "If other car is blocking you in your way you can 'jump' on it and swap controls with the other car.")
                    ]
                , paragraph []
                    [ el [ alignTop, centerX, Font.size 25 ] (Element.text "You can change amount of additional jumps, time and you can also change speed of the game in the Settings page.")
                    ]
                , paragraph []
                    [ el [ alignTop, centerX, Font.size 25 ] (Element.text "Changing these parameters affects your final score which you can see in the High Score page. If you increase jumps or duration your final high score will go down and vice versa. If you increase speed your final high score will go up and vice versa.")
                    ]
                ]
            ]
        ]


subs : Model -> Sub Msg
subs model =
    onAnimationFrameDelta ((\dt -> dt / 1000) >> Tick)
