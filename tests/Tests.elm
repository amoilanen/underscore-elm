module Tests exposing (..)

import Underscore exposing (map, reduce)

import Test exposing (..)
import Expect

all : Test
all =
    describe "Underscore.elm"
        [ describe "map"
          [ test "non-empty list" <|
            \() ->
              Expect.equal [2, 4, 6] (Underscore.map (\x -> 2 * x) [1, 2, 3])
            , test "empty list" <|
              \() ->
                Expect.equal [] (Underscore.map (\x -> 2 * x) [])
            ]
        , describe "reduce"
          [ test "non-empty list" <|
            \() ->
              Expect.equal 6 (reduce (\s x -> s + x) 0 [1, 2, 3])
            , test "empty list" <|
              \() ->
                Expect.equal 0 (reduce (\s x -> s + x) 0 [])
          ]
        ]
