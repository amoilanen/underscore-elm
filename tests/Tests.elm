module Tests exposing (..)

import Underscore exposing (
  map,
  reduce,
  reduceRight,
  find,
  filter,
  whereDict,
  whereProperty,
  findWhereDict,
  findWhereProperty,
  reject,
  every,
  some,
  contains,
  pluckDict,
  pluck,
  min,
  max)

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
              Expect.equal [Dict.fromList [(1, "1"), (2, "2")], Dict.fromList [(2, "2"), (3, "3")]] (whereDict
                (Dict.fromList [(2, "2")])
                [Dict.fromList [(1, "1"), (2, "2")], Dict.fromList [(2, "2"), (3, "3")], Dict.fromList [(3, "3"), (4, "4")]]
              )
            , test "returns empty list if list is empty" <|
            \() ->
              Expect.equal [] (whereDict (Dict.fromList [(2, "2")]) [])
          ]
        , describe "whereProperty"
          [ test "leaves only the elements matching the provided property value" <|
            \() ->
              let
                expectedValue = [
                  { city = "Helsinki", country = "Finland" }
                  , { city = "Turku", country = "Finland" }
                ]
                actualValue = (whereProperty .country "Finland" [
                  { city = "Helsinki", country = "Finland" }
                  , { city = "Turku", country = "Finland" }
                  , { city = "Tallinn", country = "Estonia" }
                ])
              in
                Expect.equal expectedValue actualValue
          ]
        , describe "findWhereDict"
          [ test "return first element if there are multiple matching elements" <|
            \() ->
              Expect.equal (Just (Dict.fromList [(1, "1"), (2, "2")])) (findWhereDict
                (Dict.fromList [(2, "2")])
                [Dict.fromList [(1, "1"), (2, "2")], Dict.fromList [(2, "2"), (3, "3")], Dict.fromList [(3, "3"), (4, "4")]]
              )
            , test "returns Nothing if there are no matching elements" <|
            \() ->
              Expect.equal Nothing (findWhereDict (Dict.fromList [(2, "2")]) [])
          ]
        , describe "findWhereProperty"
          [ test "returns first element if there are multiple matching elements" <|
            \() ->
              let
                expectedValue = Just ({ city = "Helsinki", country = "Finland" })
                actualValue = (findWhereProperty .country "Finland" [
                  { city = "Helsinki", country = "Finland" }
                  , { city = "Turku", country = "Finland" }
                  , { city = "Tallinn", country = "Estonia" }
                ])
              in
                Expect.equal expectedValue actualValue
            , test "returns Nothing if there are no matching elements" <|
            \() ->
              let
                expectedValue = Nothing
                actualValue = (findWhereProperty .country "Latvia" [
                  { city = "Helsinki", country = "Finland" }
                  , { city = "Turku", country = "Finland" }
                  , { city = "Tallinn", country = "Estonia" }
                ])
              in
                Expect.equal expectedValue actualValue
          ]
        , describe "reject"
          [ test "rejects the elements matching the provided dictionary" <|
            \() ->
              Expect.equal [1, 3] (Underscore.reject (\x -> x % 2 == 0) [1, 2, 3, 4])
            , test "returns empty list if list is empty" <|
            \() ->
              Expect.equal [] (Underscore.reject (\x -> x % 2 == 0) [])
          ]
        , describe "every"
          [ test "every element satisfies the predicate" <|
            \() ->
              Expect.equal True (Underscore.every (\x -> x % 2 == 0) [2, 4, 6])
            , test "at least one element does not satisfy the predicate" <|
            \() ->
              Expect.equal False (Underscore.every (\x -> x % 2 == 0) [2, 3, 6])
          ]
        , describe "some"
          [ test "not a single element satisfies the predicate" <|
            \() ->
              Expect.equal False (Underscore.some (\x -> x % 2 == 0) [1, 3, 5])
            , test "at least one element satisfies the predicate" <|
            \() ->
              Expect.equal True (Underscore.some (\x -> x % 2 == 0) [2, 3, 6])
          ]
        , describe "contains"
          [ test "list contains element" <|
            \() ->
              Expect.equal True (Underscore.contains 2 [1, 2, 3])
            , test "list does not contain element" <|
            \() ->
              Expect.equal False (Underscore.contains 4 [1, 2, 3])
          ]
        , describe "pluckDict"
          [ test "list of dictionaries some of which contain key" <|
            \() ->
              Expect.equal
                [Just "name1", Nothing, Just "name3"]
                (pluckDict "name" [
                  Dict.fromList [("name", "name1"), ("email", "email1")],
                  Dict.fromList [("email", "email2")],
                  Dict.fromList [("name", "name3"), ("email", "email3")]
                ])
          ]
        , describe "pluck"
          [ test "list some of which contain property" <|
            \() ->
              Expect.equal
                ["Alice", "Bob", "Chuck"]
                (pluck .name [
                  { name="Alice", height=1.62 }
                  , { name="Bob", height=1.85 }
                  , { name="Chuck", height=1.76 }
                ])
          ]
        , describe "min"
          [ test "list with a minimum element" <|
            \() ->
              Expect.equal (Just 1) (Underscore.min [2, 1, 3])
            , test "empty list" <|
            \() ->
              Expect.equal Nothing (Underscore.min [])
          ]
        , describe "max"
          [ test "list with a maximum element" <|
            \() ->
              Expect.equal (Just 3) (Underscore.max [2, 1, 3])
            , test "empty list" <|
            \() ->
              Expect.equal Nothing (Underscore.max [])
          ]
        ]