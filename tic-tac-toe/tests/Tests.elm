module Tests exposing (..)

import Test exposing (..)
import Expect

import ListExtra


-- Check out http://package.elm-lang.org/packages/elm-community/elm-test/latest to learn more about testing in Elm!


lol =
    [ [1, 2, 3]
    , [4, 5, 6]
    , [7, 8, 9]
    ]

lolT =
    [ [1, 4, 7]
    , [2, 5, 8]
    , [3, 6, 9]
    ]



all : Test
all =
    describe "A Test Suite"
        [ test "Addition" <|
            \_ ->
                Expect.equal 10 (3 + 7)
        , test "String.left" <|
            \_ ->
                Expect.equal "a" (String.left 1 "abcdefg")
        , test "Transposition" <|
            \_ ->
                Expect.equal (Just lolT) (ListExtra.transpose lol)
        ]
