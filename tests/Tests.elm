module Tests exposing (..)

import Underscore exposing (map)

import Test exposing (..)
import Expect

all : Test
all =
    describe "Underscore.elm"
        [ describe "map"
            [ test "non-empty list" <|
                \() ->
                    Expect.equal [2, 4, 6] (Underscore.map [1, 2, 3] (\x -> 2 * x))
            , test "empty list" <|
                \() ->
                    Expect.equal [] (Underscore.map [] (\x -> 2 * x))
            ]
        ]
