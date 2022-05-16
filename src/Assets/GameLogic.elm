module Assets.GameLogic exposing (..)

import Assets.Data as Data exposing (..)
import Collage exposing (..)
import Color exposing (..)
import Dict
import Dict.Extra
import Keyboard.Event exposing (KeyboardEvent)
import Keyboard.Key
import Tuple3



--update board with time


tick : Float -> Board -> Board
tick dt board =
    updateBoard board



--find next coordnita


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



--get field  or point and field for first occurance of point


get : Data.Point -> Board -> Field
get point board =
    case Dict.Extra.find (\key _ -> key == point) board.boardItself of
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



--primary function for updating animated cars


updateBoard : Board -> Board
updateBoard board =
    let
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
                Road _ ->
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
                (\key _ b -> Dict.insert key b)
                (\key b -> Dict.insert key b)
                clearedBoard
                helperMovedCars
                Dict.empty
        , remainingJumps = board.remainingJumps
    }



--rotates car clockwise


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



--draws car


carElement : Car -> Collage msg
carElement car =
    let
        rotationDegrees =
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
    if car.color == blue then
        Collage.image ( blockSize, blockSize ) "car1.svg" |> Collage.rotate (degrees rotationDegrees)

    else
        Collage.image ( blockSize, blockSize ) "car2.svg" |> Collage.rotate (degrees rotationDegrees)



-- extract car from field


takeCar : ( Data.Point, Field ) -> ( Data.Point, Car )
takeCar ( point, field ) =
    case field of
        Road car ->
            ( point, car )

        _ ->
            ( point, { movement = Animated, direction = Data.Down, color = white } )



--check if car is on given field


hasCar : Data.Point -> Field -> Bool
hasCar point field =
    case field of
        Road _ ->
            True

        RoadEmpty ->
            False

        Tile ->
            False

        Empty ->
            False



--primary function for moving key input car


moveCars : Maybe KeyboardEvent -> Board -> Board
moveCars maybeEvent board =
    let
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
                                            Dict.insert (nextCoordsKey key event.keyCode) (Road { movement = KeyInput, direction = newDir value.direction event.keyCode, color = blue }) acc

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
                    Dict.insert key (Road { movement = Animated, direction = value.direction, color = red }) acc
                )
                Dict.empty
                helperCarFieldsDict

        helperInsertedRow eventKey =
            case get (nextCoordsKey (Tuple.first justJustMovableCar) eventKey) board of
                Road _ ->
                    Dict.insert (nextCoordsKey (Tuple.first justJustMovableCar) eventKey) (Road (Tuple.second justJustMovableCar)) helperAllAnimated

                _ ->
                    Dict.insert (Tuple.first justJustMovableCar) (Road (Tuple.second justJustMovableCar)) helperAllAnimated

        helperInsertedAnimated =
            case maybeEvent of
                Just event ->
                    case event.keyCode of
                        Keyboard.Key.Up ->
                            helperInsertedRow event.keyCode

                        Keyboard.Key.Right ->
                            helperInsertedRow event.keyCode

                        Keyboard.Key.Down ->
                            helperInsertedRow event.keyCode

                        Keyboard.Key.Left ->
                            helperInsertedRow event.keyCode

                        _ ->
                            Dict.insert (Tuple.first justJustMovableCar) (Road (Tuple.second justJustMovableCar)) helperAllAnimated

                Nothing ->
                    Dict.insert (Tuple.first justJustMovableCar) (Road (Tuple.second justJustMovableCar)) helperAllAnimated

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
                    ( point, { movement = car.movement, direction = car.direction, color = blue } )

                Nothing ->
                    ( ( 10, 10 ), blackCar )

        chosenDictWinning eventKey =
            case get (nextCoordsKey (Tuple.first justJustMovableCar) eventKey) board of
                Road _ ->
                    if board.remainingJumps > 0 then
                        ( helperInsertedAnimated, board.remainingJumps - 1, True )

                    else
                        ( helperMovedCars, board.remainingJumps, False )

                _ ->
                    ( helperMovedCars, board.remainingJumps, True )

        chosenDictNotWinning eventKey =
            case get (nextCoordsKey (Tuple.first justJustMovableCar) eventKey) board of
                Road _ ->
                    if board.remainingJumps > 0 then
                        ( helperInsertedAnimated, board.remainingJumps - 1, False )

                    else
                        ( helperMovedCars, board.remainingJumps, False )

                _ ->
                    ( helperMovedCars, board.remainingJumps, False )

        chosenDictFinal eventKey =
            if nextCoordsKey (Tuple.first justJustMovableCar) eventKey == board.winningField then
                chosenDictWinning eventKey

            else
                chosenDictNotWinning eventKey

        finalMoved =
            case maybeEvent of
                Just event ->
                    case event.keyCode of
                        Keyboard.Key.Up ->
                            chosenDictFinal event.keyCode

                        Keyboard.Key.Right ->
                            chosenDictFinal event.keyCode

                        Keyboard.Key.Down ->
                            chosenDictFinal event.keyCode

                        Keyboard.Key.Left ->
                            chosenDictFinal event.keyCode

                        _ ->
                            ( carFields, board.remainingJumps, False )

                Nothing ->
                    ( carFields, board.remainingJumps, False )

        clear point field =
            case field of
                Road _ ->
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
                    (\key _ b -> Dict.insert key b)
                    (\key b -> Dict.insert key b)
                    clearedBoard
                    (Tuple3.first finalMoved)
                    Dict.empty
            , remainingJumps = Tuple3.second finalMoved
            , won = Tuple3.third finalMoved
        }
