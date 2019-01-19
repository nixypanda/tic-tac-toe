module Grid exposing (
      Grid
    , Point
    , rect
    , square
    , get
    , set
    , toList
    )

import Array exposing (Array)


type alias Grid a =
    { grid : Array (Array a)
    }


type alias Point = (Int, Int)


rect : Int -> Int -> a -> Grid a
rect length height startValue =
    let
        row = Array.repeat length startValue
    in
        { grid = Array.repeat height row }


square : Int -> a -> Grid a
square size startValue =
    rect size size startValue


set : Point -> a -> Grid a -> Grid a
set (x, y) val ({grid}) =
    Array.get y grid
    |> Maybe.map (Array.set x val)
    |> Maybe.map (\row -> Array.set y row grid)
    |> Maybe.map Grid
    |> Maybe.withDefault {grid = grid}


get : Point -> Grid a -> Maybe a
get (x, y) ({grid}) =
    Array.get y grid
        |> Maybe.andThen (\row -> Array.get x row)


toList : Grid a -> List (List a)
toList ({grid}) =
    Array.map Array.toList grid
    |> Array.toList

