module Tests exposing (..)

import Underscore exposing (map, reduce, reduceRight, find, filter, whereDict, reject)

import Dict exposing (fromList)
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
        , describe "filter"
          [ test "leaves only the elements satisfying the predicate" <|
            \() ->
              Expect.equal [2, 4] (Underscore.filter (\x -> x % 2 == 0) [1, 2, 3, 4])
            , test "returns empty list if list is empty" <|
              \() ->
                Expect.equal [] (Underscore.filter (\x -> x % 2 == 0) [1, 3, 5])
          ]
        , describe "whereDict"
          [ test "leaves only the elements matching the provided dictionary" <|
            \() ->
              Expect.equal [Dict.fromList [(1, '1'), (2, '2')], Dict.fromList [(2, '2'), (3, '3')]] (whereDict
                (Dict.fromList [(2, '2')])
                [Dict.fromList [(1, '1'), (2, '2')], Dict.fromList [(2, '2'), (3, '3')], Dict.fromList [(3, '3'), (4, '4')]]
              )
            , test "returns empty list if list is empty" <|
              \() ->
                Expect.equal [] (whereDict (Dict.fromList [(2, '2')]) [])
          ]
        , describe "reject"
          [ test "rejects the elements matching the provided dictionary" <|
            \() ->
              Expect.equal [1, 3] (Underscore.reject (\x -> x % 2 == 0) [1, 2, 3, 4])
            , test "returns empty list if list is empty" <|
              \() ->
                Expect.equal [] (Underscore.reject (\x -> x % 2 == 0) [])
          ]
        ]
