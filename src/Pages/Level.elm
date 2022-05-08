module Pages.Level exposing (..)

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
import Dict.Extra exposing (filterMap)
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


type Direction
    = Up
    | Right
    | Down
    | Left


type Movement
    = KeyInput
    | Animated


type Field
    = Road Car
    | RoadEmpty
    | Tile
    | Empty


type alias Point =
    ( Int, Int )


type alias Car =
    { movement : Movement
    , direction : Direction
    , color : Color.Color
    }



-- type alias Cars =
--     List Car


type alias Board =
    { boardItself : Dict Point Field
    , alreadyTaken : List Point
    }


initialBoard : Board
initialBoard =
    { boardItself =
        Dict.fromList
            [ ( ( 0, 0 ), Road { movement = Animated, direction = Right, color = green } )
            , ( ( 0, 1 ), Tile )
            , ( ( 0, 2 ), Tile )
            , ( ( 0, 3 ), Tile )
            , ( ( 1, 0 ), RoadEmpty )
            , ( ( 1, 1 ), RoadEmpty )
            , ( ( 1, 2 ), RoadEmpty )
            , ( ( 1, 3 ), Road { movement = Animated, direction = Right, color = red } )
            , ( ( 2, 0 ), Tile )
            , ( ( 2, 1 ), Tile )
            , ( ( 2, 2 ), RoadEmpty )
            , ( ( 2, 3 ), Tile )
            , ( ( 3, 0 ), Tile )
            , ( ( 3, 1 ), Tile )
            , ( ( 3, 2 ), RoadEmpty )
            , ( ( 3, 3 ), RoadEmpty )
            ]
    , alreadyTaken = []
    }


boardSize =
    4


blockSize =
    64


tileColor =
    lightGreen


roadColor =
    darkGray



-- Model


type alias Delta =
    Float


type alias Model =
    { localUser : User
    , lastEvent : Maybe KeyboardEvent
    , time : Float
    , board : Board
    }


init : User -> ( Model, Cmd Msg )
init user =
    ( { localUser = user, lastEvent = Nothing, time = 0, board = initialBoard }
    , Cmd.none
    )



-- initialCars : Cars
-- initialCars =
--     Dict.fromList
--         [ makeCarEntry True 1 ( 0, 0 ) Right lightBlue
--         , makeCarEntry False 2 ( 1, 1 ) Right red
--         , makeCarEntry True 3 ( 7, 2 ) Left black
--         , makeCarEntry False 4 ( 2, 4 ) Right white
--         ]
-- makeCarEntry : Bool -> Int -> Point -> Direction -> Color.Color -> ( Int, Car )
-- makeCarEntry mov index pt dir c =
--     ( index, { moving = mov, direction = dir, coords = pt, color = c, movement = Animated } )
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

                -- , cars = moveCars (Just event) model.cars
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
                | board =
                    if shouldUpdate then
                        tick dt model.board

                    else
                        model.board
                , time = newTime
              }
            , Cmd.none
            )

        NoOp ->
            ( model, Cmd.none )


tick : Float -> Board -> Board
tick dt board =
    updateBoard board


nextCoords : Direction -> Point -> Point
nextCoords dir ( x, y ) =
    case dir of
        Up ->
            ( x, y - 1 )

        Right ->
            ( x + 1, y )

        Down ->
            ( x, y + 1 )

        Left ->
            ( x - 1, y )


get : Point -> Board -> Field
get coords board =
    case Dict.Extra.find (\key _ -> key == coords) board.boardItself of
        Just ( _, tile ) ->
            tile

        Nothing ->
            Empty


updateBoard : Board -> Board
updateBoard board =
    -- Dict.map (\_ c -> updateCar c) cars
    let
        hasCar point field =
            case field of
                Road possibleCar ->
                    True

                RoadEmpty ->
                    False

                Tile ->
                    False

                Empty ->
                    False

        blackCar =
            { movement = Animated, direction = Right, color = black }

        -- funct k v =
        --     case v of
        --         Road possibleCar ->
        --             case get (nextCoords possibleCar.direction k) board of
        --                 Road nextCar ->
        --                     Just <| Road possibleCar
        --
        --                 RoadEmpty ->
        --                     Just <| RoadEmpty
        --
        --                 _ ->
        --                     Just <| Road possibleCar
        --
        --         RoadEmpty ->
        --             Just <| RoadEmpty
        --
        --         Tile ->
        --             Just <| Tile
        --
        --         Empty ->
        --             Just <| Empty
        -- _ =
        --     Debug.log "carFields :" carFields
        --
        -- _ =
        --     Debug.log "movedCars :" movedCars
        --
        -- _ =
        --     Debug.log "alreadyTaken :" board.alreadyTaken
        takeCar ( point, field ) =
            case field of
                Road car ->
                    ( point, car )

                _ ->
                    ( point, { movement = Animated, direction = Down, color = white } )

        carFields =
            Dict.filter hasCar board.boardItself

        movedCars =
            carFields
                |> Dict.toList
                |> List.map takeCar
                |> List.map (updateCar board)

        movedCarsDict =
            Dict.fromList movedCars

        helperCarFields =
            carFields
                |> Dict.toList
                |> List.map takeCar

        helperCarFieldsDict =
            Dict.fromList helperCarFields

        helperMovedCars =
            Dict.foldl
                (\key value acc ->
                    if Dict.member (nextCoords value.direction key) acc then
                        Dict.insert key (Road (rotateCar value)) acc

                    else
                        case get (nextCoords value.direction key) board of
                            RoadEmpty ->
                                Dict.insert (nextCoords value.direction key) (Road value) acc

                            _ ->
                                Dict.insert key (Road (rotateCar value)) acc
                )
                Dict.empty
                helperCarFieldsDict

        -- updatedCars =
        -- Dict.Extra.filterMap
        --     funct
        --     board.boardItself
        clear point field =
            case field of
                Road car ->
                    RoadEmpty

                RoadEmpty ->
                    RoadEmpty

                _ ->
                    field

        clearedBoard =
            Dict.map clear board.boardItself
    in
    { boardItself =
        Dict.merge
            (\key a -> Dict.insert key a)
            (\key a b -> Dict.insert key b)
            (\key b -> Dict.insert key b)
            clearedBoard
            helperMovedCars
            Dict.empty
    , alreadyTaken = []
    }



-- moveCar : Point -> Point
-- moveCar : comparable -> Field -> comparable1


moveCar point field =
    point


rotateCar : Car -> Car
rotateCar car =
    case car.direction of
        Up ->
            { car | direction = Right }

        Right ->
            { car | direction = Down }

        Down ->
            { car | direction = Left }

        Left ->
            { car | direction = Up }


updateCar : Board -> ( Point, Car ) -> ( Point, Field )
updateCar board ( point, car ) =
    let
        uCoords =
            nextCoords car.direction point
    in
    case get uCoords board of
        RoadEmpty ->
            ( uCoords, Road car )

        _ ->
            ( point, Road (rotateCar car) )



-- isRoad : Point -> Bool
-- isRoad point =
--     List.member point roads
---- VIEW ----
-- fieldColor : Point -> Color.Color
-- fieldColor point =
--     if isRoad point then
--         roadColor
--
--     else
--         groundColor


boardElement : Model -> Collage Msg
boardElement model =
    let
        rg =
            List.range 0 (boardSize - 1)

        makeTile x y =
            oneField (get ( x, y ) model.board)

        col x =
            vertical <|
                List.map
                    (makeTile x)
                    rg
    in
    horizontal <|
        List.map col rg



-- get : Point -> Board -> Field
-- get coords board =
--     case
--         Dict.get coords board
--     of
--         Just bruh ->
--             bruh
--
--         Nothing ->
--             Empty


oneField : Field -> Collage Msg
oneField field =
    let
        border =
            solid thin <| uniform black

        ground color =
            rectangle blockSize blockSize
                |> styled ( uniform color, border )
    in
    case field of
        Road car ->
            stack [ carElement car, ground roadColor ]

        RoadEmpty ->
            ground roadColor

        Tile ->
            ground tileColor

        Empty ->
            ground yellow


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

        border =
            solid thin <| uniform black

        tri =
            triangle (blockSize / 2)
                |> styled ( uniform car.color, border )

        -- Denotes direction
        ln =
            path [ ( 0, 0 - (blockSize / 2) ), ( 0, blockSize / 2 ) ]
                |> traced (solid thin (uniform black))
    in
    stack [ ln, tri ]
        |> Collage.rotate (degrees rotationRadians)



-- moveCars : Maybe KeyboardEvent -> Cars -> Cars
-- moveCars maybeEvent cars =
--     Dict.map (\_ c -> moveCar maybeEvent c) cars
--
--
-- moveCar : Maybe KeyboardEvent -> Car -> Car
-- moveCar maybeEvent car =
--     let
--         nextCoord ( x, y ) key =
--             case key of
--                 Keyboard.Key.Up ->
--                     ( x, y - 1 )
--
--                 Keyboard.Key.Right ->
--                     ( x + 1, y )
--
--                 Keyboard.Key.Down ->
--                     ( x, y + 1 )
--
--                 Keyboard.Key.Left ->
--                     ( x - 1, y )
--
--                 _ ->
--                     ( x, y )
--
--         newDir dir key =
--             case key of
--                 Keyboard.Key.Up ->
--                     Up
--
--                 Keyboard.Key.Right ->
--                     Right
--
--                 Keyboard.Key.Down ->
--                     Down
--
--                 Keyboard.Key.Left ->
--                     Left
--
--                 _ ->
--                     dir
--     in
--     case maybeEvent of
--         Just event ->
--             if car.moving then
--                 car
--
--             else
--                 { car
--                     | coords = nextCoord car.coords event.keyCode
--                     , direction = newDir car.direction event.keyCode
--                 }
--
--         Nothing ->
--             car


viewEvent : Maybe KeyboardEvent -> Element Msg
viewEvent maybeEvent =
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
                    (viewEvent model.lastEvent)
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


subs : Model -> Sub Msg
subs _ =
    onAnimationFrameDelta ((\dt -> dt / 1000) >> Tick)