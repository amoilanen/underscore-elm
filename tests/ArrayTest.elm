module ArrayTest exposing (..)

import Underscore.Array exposing (
  shuffle,
  sample,
  sampleOne,
  first,
  initial,
  last)

import Underscore.List exposing (
  toArray)

import Array exposing (fromList)
import Random exposing (initialSeed)

import Test exposing (..)
import Expect

all : Test
all =
    describe "Underscore.elm"
        [ describe "shuffle"
          [ test "should shuffle array" <|
            \() ->
              let
                randomSeed = initialSeed 123
                expectedValue =  Array.fromList [4, 1, 5, 2, 3, 6]
                actualValue = shuffle (Array.fromList [1, 2, 3, 4, 5, 6]) randomSeed
              in
                Expect.equal expectedValue actualValue
            , test "different seeds produce different shufflings" <|
            \() ->
              let
                randomSeed = initialSeed 123
                otherRandomSeed= initialSeed 234
                arr = Array.fromList [1, 2, 3, 4, 5, 6]
              in
                Expect.notEqual (shuffle arr randomSeed) (shuffle arr otherRandomSeed)
          ]
        , describe "sample"
          [
            describe "sample size smaller than array length" [
              test "should sample array" <|
              \() ->
                let
                  randomSeed = initialSeed 123
                  expectedValue =  Array.fromList [2, 3, 6]
                  actualValue = sample (Array.fromList [1, 2, 3, 4, 5, 6]) 3 randomSeed
                in
                  Expect.equal expectedValue actualValue
            ],
            describe "sample size greater than array length" [
              test "should sample array" <|
              \() ->
                let
                  randomSeed = initialSeed 123
                  expectedValue =  Array.fromList [4, 1, 5, 2, 3, 6]
                  actualValue = sample (Array.fromList [1, 2, 3, 4, 5, 6]) 10 randomSeed
                in
                  Expect.equal expectedValue actualValue
            ],
            describe "sample size is 1" [
              test "should sample array" <|
              \() ->
                let
                  randomSeed = initialSeed 123
                  expectedValue =  Array.fromList [6]
                  actualValue = sample (Array.fromList [1, 2, 3, 4, 5, 6]) 1 randomSeed
                in
                  Expect.equal expectedValue actualValue
            ],
            describe "sampleOne" [
              test "should sample array" <|
              \() ->
                let
                  randomSeed = initialSeed 123
                  expectedValue =  Just 6
                  actualValue = sampleOne (Array.fromList [1, 2, 3, 4, 5, 6]) randomSeed
                in
                  Expect.equal expectedValue actualValue
            ],
            describe "sample size is negative" [
              test "should sample array" <|
              \() ->
                let
                  randomSeed = initialSeed 123
                  expectedValue =  Array.fromList []
                  actualValue = sample (Array.fromList [1, 2, 3, 4, 5, 6]) -3 randomSeed
                in
                  Expect.equal expectedValue actualValue
            ]
          ]
        , describe "first"
          [ test "takes first n elements if n less than list length" <|
            \() ->
              Expect.equal (toArray [1, 2, 3]) (first 3 (toArray [1, 2, 3, 4, 5]) ),
            test "takes all list elements if n greater than list length" <|
            \() ->
              Expect.equal (toArray [1, 2, 3]) (first 5 (toArray [1, 2, 3]) ),
            test "takes no elements if n is not positive" <|
            \() ->
              Expect.equal (toArray []) (first -3 (toArray [1, 2, 3]) )
          ]
        , describe "initial"
          [ test "takes first length - n elements if n less than list length" <|
            \() ->
              Expect.equal (toArray [1, 2]) (initial 3 (toArray [1, 2, 3, 4, 5]) ),
            test "takes all list elements if n is negative" <|
            \() ->
              Expect.equal (toArray [1, 2, 3]) (initial -3 (toArray [1, 2, 3]) ),
            test "takes all elements if n is 0" <|
            \() ->
              Expect.equal (toArray [1, 2, 3]) (initial 0 (toArray [1, 2, 3]) )
          ]
        , describe "last"
          [ test "takes last n elements if n less than list length" <|
            \() ->
              Expect.equal (toArray [3, 4, 5]) (last 3 (toArray [1, 2, 3, 4, 5]) ),
            test "takes no elements if n is negative" <|
            \() ->
              Expect.equal (toArray []) (last -3 (toArray [1, 2, 3]) ),
            test "takes all elements if n exceeds list length" <|
            \() ->
              Expect.equal (toArray [1, 2, 3]) (last 5 (toArray [1, 2, 3]) )
          ]
        ]