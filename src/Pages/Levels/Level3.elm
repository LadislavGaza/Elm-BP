module Pages.Levels.Level3 exposing (..)

import Assets.Data as Data exposing (..)
import Assets.GameLogic exposing (..)
import Assets.Style exposing (..)
import Browser.Events exposing (onAnimationFrameDelta)
import Collage exposing (..)
import Collage.Layout exposing (..)
import Collage.Render exposing (svg)
import Color exposing (..)
import Dict
import Element exposing (..)
import Element.Background as Background
import Element.Font as Font
import Html.Attributes exposing (autofocus, tabindex)
import Html.Events exposing (on)
import Json.Decode as Json
import Keyboard.Event exposing (KeyboardEvent, decodeKeyboardEvent)
import List
import Round
import String



----MODEL----


amountOfJumps =
    2


winningFieldPoint =
    ( 13, 1 )


boardSizeX =
    14


boardSizeY =
    2


gameDuration =
    10


initialBoard : User -> Board
initialBoard user =
    { boardItself =
        Dict.fromList
            [ ( ( 0, 0 ), Road { movement = KeyInput, direction = Data.Right, color = blue } )
            , ( ( 0, 1 ), RoadEmpty )
            , ( ( 1, 0 ), Road { movement = Animated, direction = Data.Right, color = red } )
            , ( ( 1, 1 ), RoadEmpty )
            , ( ( 2, 0 ), RoadEmpty )
            , ( ( 2, 1 ), Road { movement = Animated, direction = Data.Left, color = red } )
            , ( ( 3, 0 ), RoadEmpty )
            , ( ( 3, 1 ), RoadEmpty )
            , ( ( 4, 0 ), Road { movement = Animated, direction = Data.Right, color = red } )
            , ( ( 4, 1 ), RoadEmpty )
            , ( ( 5, 0 ), RoadEmpty )
            , ( ( 5, 1 ), RoadEmpty )
            , ( ( 6, 0 ), RoadEmpty )
            , ( ( 6, 1 ), RoadEmpty )
            , ( ( 7, 0 ), RoadEmpty )
            , ( ( 7, 1 ), Road { movement = Animated, direction = Data.Left, color = red } )
            , ( ( 8, 0 ), Road { movement = Animated, direction = Data.Right, color = red } )
            , ( ( 8, 1 ), RoadEmpty )
            , ( ( 9, 0 ), RoadEmpty )
            , ( ( 9, 1 ), Road { movement = Animated, direction = Data.Left, color = red } )
            , ( ( 10, 0 ), RoadEmpty )
            , ( ( 10, 1 ), RoadEmpty )
            , ( ( 11, 0 ), Road { movement = Animated, direction = Data.Right, color = red } )
            , ( ( 11, 1 ), RoadEmpty )
            , ( ( 12, 0 ), RoadEmpty )
            , ( ( 12, 1 ), RoadEmpty )
            , ( ( 13, 0 ), Road { movement = Animated, direction = Data.Down, color = red } )
            , ( ( 13, 1 ), RoadEmpty )
            ]
    , remainingJumps = amountOfJumps + user.extraJumps
    , winningField = winningFieldPoint
    , won = False
    }



-- Model


type Msg
    = HandleKeyboardEvent KeyboardEvent
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

                newTimeDt =
                    model.time + newDt

                secsPassed =
                    round model.time

                newSecs =
                    round newTimeDt

                shouldUpdate =
                    secsPassed /= newSecs

                newMaxScore =
                    100 + model.board.remainingJumps * 10 - model.localUser.extraJumps * 2 - (gameDuration + model.localUser.extraDuration) + model.localUser.extraGameSpeed * 10

                newUser =
                    { extraJumps = model.localUser.extraJumps
                    , extraGameSpeed = model.localUser.extraGameSpeed
                    , extraDuration = model.localUser.extraDuration
                    , level1HS = model.localUser.level1HS
                    , level2HS = model.localUser.level2HS
                    , level3HS =
                        if model.localUser.level3HS < newMaxScore then
                            newMaxScore

                        else
                            model.localUser.level3HS
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
        carsElement =
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
                [ el [ alignTop, centerX, Font.size 50 ] (Element.text "Level 3")
                , el [ alignTop, centerX ] carsElement
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
