module Tests exposing (..)

import Underscore exposing (map, reduce, reduceRight, find)

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
        , describe "reduceRight"
          [ test "non-empty list" <|
            \() ->
              Expect.equal "123" (reduceRight (\s x -> s ++ x) "" ["1", "2", "3"])
            , test "empty list" <|
              \() ->
                Expect.equal "" (reduceRight (\s x -> s ++ x) "" [])
          ]
        , describe "find"
          [ test "finds first element satisfying the predicate" <|
            \() ->
              Expect.equal (Just 2) (find (\x -> x % 2 == 0) [1, 2, 3, 4])
            , test "returns Nothing if no element is found" <|
              \() ->
                Expect.equal Nothing (find (\x -> x % 2 == 0) [1, 3, 5])
            , test "returns Nothing if list is empty" <|
              \() ->
                Expect.equal Nothing (find (\x -> x % 2 == 0) [])
          ]
        ]
