module Pages.Settings exposing (..)

import Browser exposing (element)
import Browser.Dom as Dom
import Browser.Events exposing (onAnimationFrameDelta)
import Char exposing (..)
import Collage exposing (..)
import Collage.Layout exposing (..)
import Collage.Render exposing (svg)
import Color exposing (..)
import Data exposing (..)
import Dict exposing (Dict)
import Dict.Extra as Extra
import Element as Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Element.Region as Region
import Html exposing (Attribute, Html, div, h1, h3, p, pre, text)
import Html.Attributes exposing (id, style, tabindex)
import Html.Events exposing (on)
import Json.Decode as Json exposing (..)
import Json.Encode as Encode exposing (..)
import Keyboard
import Keyboard.Event exposing (KeyboardEvent, decodeKeyboardEvent)
import Keyboard.Key
import List
import String exposing (..)
import Task



----MODEL----


roads : List Point
roads =
    [ ( 0, 0 )
    , ( 1, 0 )
    , ( 2, 0 )
    , ( 3, 0 )
    , ( 4, 0 )
    , ( 4, 1 )
    , ( 4, 2 )
    , ( 4, 3 )
    , ( 4, 4 )
    , ( 5, 4 )
    , ( 6, 4 )
    , ( 7, 4 )
    ]


boardSize =
    8


blockSize =
    64


groundColor =
    lightGreen


roadColor =
    darkGray


type alias Point =
    ( Int, Int )


type Direction
    = Up
    | Right
    | Down
    | Left


type Movement
    = KeyInput
    | Animated


type alias Car =
    { coords : Point
    , moving : Bool
    , direction : Direction
    , color : Color.Color
    , movement : Movement
    }


type alias Cars =
    Dict Int Car



-- Model


type alias Delta =
    Float


type alias Model =
    { localUser : User
    , msg : String
    , lastEvent : Maybe KeyboardEvent
    , cars : Cars
    , time : Float
    }


init : User -> ( Model, Cmd Msg )
init user =
    ( { localUser = user, msg = "", lastEvent = Nothing, cars = initialCars, time = 0 }
    , Cmd.none
    )


initialCars : Cars
initialCars =
    Dict.fromList
        [ makeCarEntry True 1 ( 0, 0 ) Right lightBlue
        , makeCarEntry False 2 ( 1, 1 ) Right red
        , makeCarEntry True 3 ( 7, 2 ) Left black
        , makeCarEntry False 4 ( 2, 4 ) Right white
        ]


makeCarEntry : Bool -> Int -> Point -> Direction -> Color.Color -> ( Int, Car )
makeCarEntry mov index pt dir c =
    ( index, { moving = mov, direction = dir, coords = pt, color = c, movement = Animated } )



---UPDATE----
-- type Msg
--     = Name String
--     | Levels Int
--     | UpdateInfo User


type Msg
    = HandleKeyboardEvent KeyboardEvent
    | NoOp
    | Tick Delta


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        HandleKeyboardEvent event ->
            ( { model
                | lastEvent = Just event
                , cars = moveCars (Just event) model.cars
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
            in
            ( { model
                | cars =
                    if shouldUpdate then
                        tick dt model.cars

                    else
                        model.cars
                , time = newTime
              }
            , Cmd.none
            )

        NoOp ->
            ( model, Cmd.none )


tick : Float -> Cars -> Cars
tick dt cars =
    Dict.map (\_ c -> updateCar c) cars


updateCar : Car -> Car
updateCar car =
    let
        nextCoords ( x, y ) =
            case car.direction of
                Up ->
                    ( x, y + 1 )

                Right ->
                    ( x + 1, y )

                Down ->
                    ( x, y - 1 )

                Left ->
                    ( x - 1, y )
    in
    if car.moving then
        { car | coords = nextCoords car.coords }

    else
        car


isRoad : Point -> Bool
isRoad point =
    List.member point roads


subs : Model -> Sub Msg
subs _ =
    onAnimationFrameDelta ((\dt -> dt / 1000) >> Tick)



---- VIEW ----


border : LineStyle
border =
    solid thin <| uniform black


tileColor : Point -> Color.Color
tileColor point =
    if isRoad point then
        roadColor

    else
        groundColor


boardElement : Model -> Collage msg
boardElement model =
    let
        rg =
            List.range 0 (boardSize - 1)

        makeTile x y =
            case Extra.find (\_ car -> car.coords == ( x, y )) model.cars of
                Just ( _, car ) ->
                    stack [ carElement car, tileElement <| tileColor ( x, y ) ]

                Nothing ->
                    tileElement <| tileColor ( x, y )

        col x =
            vertical <|
                List.map
                    (makeTile x)
                    rg
    in
    horizontal <|
        List.map col rg


tileElement : Color.Color -> Collage msg
tileElement c =
    rectangle blockSize blockSize
        |> styled ( uniform c, border )


carElement : Car -> Collage msg
carElement car =
    let
        rotationRadians =
            case car.direction of
                Up ->
                    0

                Right ->
                    270

                Down ->
                    180

                Left ->
                    90

        tri =
            triangle (blockSize / 2)
                |> styled ( uniform car.color, border )

        -- Denotes direction
    in
    tri
        |> Collage.rotate (degrees rotationRadians)


viewEvent : Maybe KeyboardEvent -> Cars -> Element Msg
viewEvent maybeEvent cars =
    case maybeEvent of
        Just event ->
            case event.keyCode of
                Keyboard.Key.Up ->
                    el []
                        (Element.text
                            ("keyCode: UPPPP"
                                ++ Debug.toString event.keyCode
                            )
                        )

                Keyboard.Key.Down ->
                    el []
                        (Element.text
                            ("keyCode: DOWN "
                                ++ Debug.toString event.keyCode
                            )
                        )

                _ ->
                    el []
                        (Element.text
                            ("keyCode: "
                                ++ Debug.toString event.keyCode
                            )
                        )

        Nothing ->
            el [] (Element.text "No event yet")


moveCars : Maybe KeyboardEvent -> Cars -> Cars
moveCars maybeEvent cars =
    Dict.map (\_ c -> moveCar maybeEvent c) cars


moveCar : Maybe KeyboardEvent -> Car -> Car
moveCar maybeEvent car =
    let
        nextCoord ( x, y ) key =
            case key of
                Keyboard.Key.Up ->
                    ( x, y - 1 )

                Keyboard.Key.Right ->
                    ( x + 1, y )

                Keyboard.Key.Down ->
                    ( x, y + 1 )

                Keyboard.Key.Left ->
                    ( x - 1, y )

                _ ->
                    ( x, y )

        newDir dir key =
            case key of
                Keyboard.Key.Up ->
                    Up

                Keyboard.Key.Right ->
                    Right

                Keyboard.Key.Down ->
                    Down

                Keyboard.Key.Left ->
                    Left

                _ ->
                    dir
    in
    case maybeEvent of
        Just event ->
            if car.moving then
                car

            else
                { car
                    | coords = nextCoord car.coords event.keyCode
                    , direction = newDir car.direction event.keyCode
                }

        Nothing ->
            car


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
            ]
            [ column [ alignLeft, alignTop, centerX, Element.height fill, Element.width (px 400), paddingXY 20 20, spacing 15 ]
                [ el [ alignTop, centerX, Font.size 50 ] (Element.text "Level")
                , el
                    [ alignTop
                    , centerX
                    , Font.size 50
                    ]
                    (viewEvent model.lastEvent model.cars)
                , el [ centerX ] auticka
                , Element.link buttonStyle
                    { label = Element.text "Home"
                    , url = "Home"
                    }
                ]
            ]
        ]


buttonStyle : List (Element.Attribute msg)
buttonStyle =
    [ Element.width (px 300)
    , Background.color (Element.rgb255 57 124 213)
    , Font.color (Element.rgb 1 1 1)
    , paddingXY 14 10

    -- , style "margin-top" "10px"
    -- , style "margin-left" "10px"
    , Border.rounded 10
    , Font.size 20
    , Font.center
    , centerX
    ]
