module Grid exposing (
      Grid
    , Point
    , rect
    , square
    , get
    , set
    , toList
    , allRows
    , allColumns
    , mainDiagonal
    , secondaryDiagonal
    , unsafeTranspose

    )

import Array exposing (Array)
import ListExtra


type alias Grid a =
    { grid : Array (Array a)
    , size : {height : Int, width : Int}
    }


type alias Point = (Int, Int)


rect : Int -> Int -> a -> Grid a
rect height width startValue =
    let
        row = Array.repeat width startValue
        size = {height = height, width = width}
    in
        { grid = Array.repeat height row, size = size }


square : Int -> a -> Grid a
square size startValue =
    rect size size startValue


set : Point -> a -> Grid a -> Grid a
set (x, y) val ({grid} as model) =
    Array.get y grid
    |> Maybe.map (Array.set x val)
    |> Maybe.map (\row -> Array.set y row grid)
    |> Maybe.map (\g -> {model | grid = g})
    |> Maybe.withDefault {model | grid = grid}


get : Point -> Grid a -> Maybe a
get (x, y) ({grid}) =
    Array.get y grid
        |> Maybe.andThen (\row -> Array.get x row)


toList : Grid a -> List (List a)
toList ({grid}) =
    Array.map Array.toList grid
    |> Array.toList


allRows : Grid a -> List (List a)
allRows = toList


allColumns : Grid a -> List (List a)
allColumns = toList >> unsafeTranspose


mainDiagonal : Grid a -> List a
mainDiagonal = toList >> unsafeMainDiagonal


secondaryDiagonal : Grid a -> List a
secondaryDiagonal = toList >> unsafeSecondaryDiagonal


unsafeTranspose : List (List a) -> (List (List a))
unsafeTranspose = Maybe.withDefault [] << ListExtra.transpose


unsafeMainDiagonal : List (List a) -> List a
unsafeMainDiagonal = Maybe.withDefault [] << ListExtra.mainDiagonal


unsafeSecondaryDiagonal : List (List a) -> List a
unsafeSecondaryDiagonal = Maybe.withDefault [] << ListExtra.secondaryDiagonal
