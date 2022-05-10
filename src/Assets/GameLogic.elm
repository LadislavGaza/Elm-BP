module Assets.GameLogic exposing (..)

import Collage exposing (..)
import Color exposing (..)
import Data exposing (..)
import Dict exposing (Dict)
import Dict.Extra exposing (..)
import Keyboard.Event exposing (KeyboardEvent)
import Keyboard.Key
import Tuple3 exposing (..)


tick : Float -> Board -> Board
tick dt board =
    updateBoard board


nextCoords : Data.Direction -> Data.Point -> Data.Point
nextCoords dir ( x, y ) =
    case dir of
        Data.Up ->
            ( x, y - 1 )

        Data.Right ->
            ( x + 1, y )

        Data.Down ->
            ( x, y + 1 )

        Data.Left ->
            ( x - 1, y )


get : Data.Point -> Board -> Field
get coords board =
    case Dict.Extra.find (\key _ -> key == coords) board.boardItself of
        Just ( _, tile ) ->
            tile

        Nothing ->
            Empty


getPointField : Data.Point -> Board -> ( Data.Point, Field )
getPointField coords board =
    case Dict.Extra.find (\key _ -> key == coords) board.boardItself of
        Just ( _, tile ) ->
            ( coords, tile )

        Nothing ->
            ( ( -1, -1 ), Empty )


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

        -- blackCar =
        --     { movement = Animated, direction = Right, color = black }
        takeCar ( point, field ) =
            case field of
                Road car ->
                    ( point, car )

                _ ->
                    ( point, { movement = Animated, direction = Data.Down, color = white } )

        carFields =
            Dict.filter hasCar board.boardItself

        helperCarFields =
            carFields
                |> Dict.toList
                |> List.map takeCar

        helperCarFieldsDict =
            Dict.fromList helperCarFields

        helperMovedCars =
            Dict.foldl
                (\key value acc ->
                    case value.movement of
                        Animated ->
                            if Dict.member (nextCoords value.direction key) acc then
                                Dict.insert key (Road (rotateCar value)) acc

                            else
                                case get (nextCoords value.direction key) board of
                                    RoadEmpty ->
                                        Dict.insert (nextCoords value.direction key) (Road value) acc

                                    _ ->
                                        Dict.insert key
                                            (Road (rotateCar value))
                                            acc

                        KeyInput ->
                            Dict.insert key (Road value) acc
                )
                Dict.empty
                helperCarFieldsDict

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
    { board
        | boardItself =
            Dict.merge
                (\key a -> Dict.insert key a)
                (\key a b -> Dict.insert key b)
                (\key b -> Dict.insert key b)
                clearedBoard
                helperMovedCars
                Dict.empty
        , remainingJumps = board.remainingJumps
    }


rotateCar : Car -> Car
rotateCar car =
    case car.direction of
        Data.Up ->
            { car | direction = Data.Right }

        Data.Right ->
            { car | direction = Data.Down }

        Data.Down ->
            { car | direction = Data.Left }

        Data.Left ->
            { car | direction = Data.Up }


carElement : Car -> Collage msg
carElement car =
    let
        rotationRadians =
            case car.direction of
                Data.Up ->
                    0

                Data.Right ->
                    270

                Data.Down ->
                    180

                Data.Left ->
                    90
    in
    if car.color == green then
        Collage.image ( blockSize, blockSize ) "/car2.svg" |> Collage.rotate (degrees rotationRadians)

    else
        Collage.image ( blockSize, blockSize ) "/car1.svg" |> Collage.rotate (degrees rotationRadians)


moveCars : Maybe KeyboardEvent -> Board -> Board
moveCars maybeEvent board =
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

        hasMovableCar point field =
            case field of
                Road possibleCar ->
                    case possibleCar.movement of
                        Animated ->
                            False

                        KeyInput ->
                            True

                RoadEmpty ->
                    False

                Tile ->
                    False

                Empty ->
                    False

        blackCar =
            { movement = Animated, direction = Data.Right, color = black }

        takeCar ( point, field ) =
            case field of
                Road car ->
                    ( point, car )

                _ ->
                    ( point, { movement = Animated, direction = Data.Down, color = white } )

        nextCoordsKey ( x, y ) key =
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
                    Data.Up

                Keyboard.Key.Right ->
                    Data.Right

                Keyboard.Key.Down ->
                    Data.Down

                Keyboard.Key.Left ->
                    Data.Left

                _ ->
                    dir

        carFields =
            Dict.filter hasCar board.boardItself

        helperCarFields =
            carFields
                |> Dict.toList
                |> List.map takeCar

        helperCarFieldsDict =
            Dict.fromList helperCarFields

        helperMovedCars =
            Dict.foldl
                (\key value acc ->
                    case value.movement of
                        Animated ->
                            Dict.insert key (Road value) acc

                        KeyInput ->
                            case maybeEvent of
                                Just event ->
                                    case get (nextCoordsKey key event.keyCode) board of
                                        RoadEmpty ->
                                            Dict.insert (nextCoordsKey key event.keyCode) (Road { movement = KeyInput, direction = newDir value.direction event.keyCode, color = green }) acc

                                        _ ->
                                            Dict.insert key (Road { movement = KeyInput, direction = newDir value.direction event.keyCode, color = value.color }) acc

                                Nothing ->
                                    Dict.insert key (Road value) acc
                )
                Dict.empty
                helperCarFieldsDict

        helperAllAnimated =
            Dict.foldl
                (\key value acc ->
                    -- Dict.insert key (Road { movement = Animated, direction = value.direction, color = value.color }) acc
                    Dict.insert key (Road { movement = Animated, direction = value.direction, color = red }) acc
                )
                Dict.empty
                helperCarFieldsDict

        insorted =
            case maybeEvent of
                Just event ->
                    case event.keyCode of
                        Keyboard.Key.Up ->
                            case get (nextCoordsKey (Tuple.first justJustMovableCar) event.keyCode) board of
                                Road car ->
                                    Dict.insert (nextCoordsKey (Tuple.first justJustMovableCar) event.keyCode) (Road (Tuple.second justJustMovableCar)) helperAllAnimated

                                _ ->
                                    Dict.insert (Tuple.first justJustMovableCar) (Road (Tuple.second justJustMovableCar)) helperAllAnimated

                        Keyboard.Key.Right ->
                            case get (nextCoordsKey (Tuple.first justJustMovableCar) event.keyCode) board of
                                Road car ->
                                    Dict.insert (nextCoordsKey (Tuple.first justJustMovableCar) event.keyCode) (Road (Tuple.second justJustMovableCar)) helperAllAnimated

                                _ ->
                                    Dict.insert (Tuple.first justJustMovableCar) (Road (Tuple.second justJustMovableCar)) helperAllAnimated

                        Keyboard.Key.Down ->
                            case get (nextCoordsKey (Tuple.first justJustMovableCar) event.keyCode) board of
                                Road car ->
                                    Dict.insert (nextCoordsKey (Tuple.first justJustMovableCar) event.keyCode) (Road (Tuple.second justJustMovableCar)) helperAllAnimated

                                _ ->
                                    Dict.insert (Tuple.first justJustMovableCar) (Road (Tuple.second justJustMovableCar)) helperAllAnimated

                        Keyboard.Key.Left ->
                            case get (nextCoordsKey (Tuple.first justJustMovableCar) event.keyCode) board of
                                Road car ->
                                    Dict.insert (nextCoordsKey (Tuple.first justJustMovableCar) event.keyCode) (Road (Tuple.second justJustMovableCar)) helperAllAnimated

                                _ ->
                                    Dict.insert (Tuple.first justJustMovableCar) (Road (Tuple.second justJustMovableCar)) helperAllAnimated

                        _ ->
                            Dict.insert (Tuple.first justJustMovableCar) (Road (Tuple.second justJustMovableCar)) helperAllAnimated

                Nothing ->
                    Dict.insert (Tuple.first justJustMovableCar) (Road (Tuple.second justJustMovableCar)) helperAllAnimated

        _ =
            Debug.log "helperAllAnimated :" helperAllAnimated

        movableCarField =
            Dict.filter hasMovableCar board.boardItself

        justMovableCar =
            movableCarField
                |> Dict.toList
                |> List.map takeCar
                |> List.head

        justJustMovableCar =
            case justMovableCar of
                Just ( point, car ) ->
                    ( point, { movement = car.movement, direction = car.direction, color = green } )

                Nothing ->
                    ( ( 10, 10 ), blackCar )

        _ =
            Debug.log "justMovableCar :" justMovableCar

        _ =
            Debug.log "insorted:"
                insorted

        finalFinal =
            case maybeEvent of
                Just event ->
                    case event.keyCode of
                        Keyboard.Key.Up ->
                            if nextCoordsKey (Tuple.first justJustMovableCar) event.keyCode == board.winningField then
                                case get (nextCoordsKey (Tuple.first justJustMovableCar) event.keyCode) board of
                                    Road car ->
                                        if board.remainingJumps > 0 then
                                            ( insorted, board.remainingJumps - 1, True )

                                        else
                                            ( helperMovedCars, board.remainingJumps, False )

                                    _ ->
                                        ( helperMovedCars, board.remainingJumps, True )

                            else
                                case get (nextCoordsKey (Tuple.first justJustMovableCar) event.keyCode) board of
                                    Road car ->
                                        if board.remainingJumps > 0 then
                                            ( insorted, board.remainingJumps - 1, False )

                                        else
                                            ( helperMovedCars, board.remainingJumps, False )

                                    _ ->
                                        ( helperMovedCars, board.remainingJumps, False )

                        Keyboard.Key.Right ->
                            if nextCoordsKey (Tuple.first justJustMovableCar) event.keyCode == board.winningField then
                                case get (nextCoordsKey (Tuple.first justJustMovableCar) event.keyCode) board of
                                    Road car ->
                                        if board.remainingJumps > 0 then
                                            ( insorted, board.remainingJumps - 1, True )

                                        else
                                            ( helperMovedCars, board.remainingJumps, False )

                                    _ ->
                                        ( helperMovedCars, board.remainingJumps, True )

                            else
                                case get (nextCoordsKey (Tuple.first justJustMovableCar) event.keyCode) board of
                                    Road car ->
                                        if board.remainingJumps > 0 then
                                            ( insorted, board.remainingJumps - 1, False )

                                        else
                                            ( helperMovedCars, board.remainingJumps, False )

                                    _ ->
                                        ( helperMovedCars, board.remainingJumps, False )

                        Keyboard.Key.Down ->
                            if nextCoordsKey (Tuple.first justJustMovableCar) event.keyCode == board.winningField then
                                case get (nextCoordsKey (Tuple.first justJustMovableCar) event.keyCode) board of
                                    Road car ->
                                        if board.remainingJumps > 0 then
                                            ( insorted, board.remainingJumps - 1, True )

                                        else
                                            ( helperMovedCars, board.remainingJumps, False )

                                    _ ->
                                        ( helperMovedCars, board.remainingJumps, True )

                            else
                                case get (nextCoordsKey (Tuple.first justJustMovableCar) event.keyCode) board of
                                    Road car ->
                                        if board.remainingJumps > 0 then
                                            ( insorted, board.remainingJumps - 1, False )

                                        else
                                            ( helperMovedCars, board.remainingJumps, False )

                                    _ ->
                                        ( helperMovedCars, board.remainingJumps, False )

                        Keyboard.Key.Left ->
                            if nextCoordsKey (Tuple.first justJustMovableCar) event.keyCode == board.winningField then
                                case get (nextCoordsKey (Tuple.first justJustMovableCar) event.keyCode) board of
                                    Road car ->
                                        if board.remainingJumps > 0 then
                                            ( insorted, board.remainingJumps - 1, True )

                                        else
                                            ( helperMovedCars, board.remainingJumps, False )

                                    _ ->
                                        ( helperMovedCars, board.remainingJumps, True )

                            else
                                case get (nextCoordsKey (Tuple.first justJustMovableCar) event.keyCode) board of
                                    Road car ->
                                        if board.remainingJumps > 0 then
                                            ( insorted, board.remainingJumps - 1, False )

                                        else
                                            ( helperMovedCars, board.remainingJumps, False )

                                    _ ->
                                        ( helperMovedCars, board.remainingJumps, False )

                        _ ->
                            ( carFields, board.remainingJumps, False )

                Nothing ->
                    ( carFields, board.remainingJumps, False )

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
    if board.won then
        board

    else
        { board
            | boardItself =
                Dict.merge
                    (\key a -> Dict.insert key a)
                    (\key a b -> Dict.insert key b)
                    (\key b -> Dict.insert key b)
                    clearedBoard
                    (Tuple3.first finalFinal)
                    Dict.empty
            , remainingJumps = Tuple3.second finalFinal
            , won = Tuple3.third finalFinal
        }
