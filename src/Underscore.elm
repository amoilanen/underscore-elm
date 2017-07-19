module Underscore exposing (
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
  max,
  sortBy,
  groupBy,
  indexBy,
  shuffleArr,
  sampleArr,
  sampleArrOne,
  shuffle,
  sample,
  sampleOne,
  toArray,
  size,
  partition,
  first,
  firstArr,
  firstOne,
  firstArrOne,
  initialArr,
  initialArrOne,
  lastArr,
  lastArrOne)

{-| Port to Elm of Underscore 1.8.3 functions.

@docs map
@docs reduce
@docs reduceRight
@docs find
@docs filter
@docs whereDict
@docs whereProperty
@docs findWhereDict
@docs findWhereProperty
@docs reject
@docs every
@docs some
@docs contains
@docs pluck
@docs pluckDict
@docs min
@docs max
@docs sortBy
@docs groupBy
@docs indexBy
@docs shuffleArr
@docs sampleArr
@docs sampleArrOne
@docs shuffle
@docs sample
@docs sampleOne
@docs toArray
@docs size
@docs partition
@docs first
@docs firstArr
@docs firstOne
@docs firstArrOne
@docs initialArr
@docs initialArrOne
@docs lastArr
@docs lastArrOne
-}

import List exposing (map, foldl, filter, all)
import Array exposing (Array, append, fromList, get, slice, length, push)
import Dict exposing (Dict, keys, get, update)
import Random exposing (Seed, int, step)

{-| Transforms a given list by applying the provided element transformation function to every element.

    map [1, 2, 3] (\x -> x > 1) == [False, True, True]
-}
map : (a -> b) -> List a -> List b
map = List.map

{-| Reduces the list from the left given the initial value and the reduction step definition.

    reduce (\s x -> s + x) 0 [1, 2, 3] == 6
-}
reduce : (a -> b -> b) -> b -> List a -> b
reduce = List.foldl

{-| Reduces the list given from the right the initial value and the reduction step definition.

    reduceRight (\s x -> s ++ x) "" ["1", "2", "3"] == "123"
-}
reduceRight : (a -> b -> b) -> b -> List a -> b
reduceRight = List.foldr

{-| Finds first element in the list that satisfies the given predicate.

    find (\x -> x > 1) [1, 2, 3] == 2
-}
find : (a -> Bool) -> List a -> Maybe a
find predicate list =
  List.head (List.filter predicate list)

{-| Returns the list of the elements of the list that satisfy the given predicate.

    filter (\x -> x > 1) [1, 2, 3] == [2, 3]
-}
filter : (a -> Bool) -> List a -> List a
filter = List.filter

{-| Returns the list of the dictionaries contained in the list that contain the given dictionary as a subdictionary.

    (whereDict
      (Dict.fromList [(2, "2")])
      [Dict.fromList [(1, "1"), (2, "2")], Dict.fromList [(2, "2"), (3, "3")], Dict.fromList [(3, "3"), (4, "4")]]) ==
    [Dict.fromList [(1, "1"), (2, "2")], Dict.fromList [(2, "2"), (3, "3")]]
-}
whereDict : Dict comparable v -> List (Dict comparable v) -> List (Dict comparable v)
whereDict pairs list =
  let
    pairKeys = Dict.keys pairs
  in
    filter (\item ->
      let
        keyValueMatchesCheck = (\key -> (Dict.get key item) == (Dict.get key pairs))
      in
        List.all keyValueMatchesCheck pairKeys
    ) list

{-| Returns the list containing the records where the property has the given value.
This function should have been called "where", but it is a reserved keyword in Elm,
then it is "whereProperty".

....(whereProperty .country "Finland" [
      { city = "Helsinki", country = "Finland" }
      , { city = "Turku", country = "Finland" }
      , { city = "Tallinn", country = "Estonia" }
    ]) == [
      { city = "Helsinki", country = "Finland" }
      , { city = "Turku", country = "Finland" }
    ]
-}
whereProperty : (a -> comparable) -> comparable -> List a -> List a
whereProperty property propertyValue list = filter (\item -> (property item) == propertyValue ) list

{-| Returns the first matching value in the list that contain the given dictionary as a subdictionary.

    (findWhereDict
      (Dict.fromList [(2, "2")])
      [Dict.fromList [(1, "1"), (2, "2")], Dict.fromList [(2, "2"), (3, "3")], Dict.fromList [(3, "3"), (4, "4")]]) ==
    Just (Dict.fromList [(1, "1"), (2, "2")])
-}
findWhereDict : Dict comparable v -> List (Dict comparable v) -> Maybe (Dict comparable v)
findWhereDict pairs list =
  List.head (whereDict pairs list)

{-| Returns the first matching value in the list where the property has the given value.

....(findWhereProperty .country "Finland" [
      { city = "Helsinki", country = "Finland" }
      , { city = "Turku", country = "Finland" }
      , { city = "Tallinn", country = "Estonia" }
    ]) == Just ({ city = "Helsinki", country = "Finland" })
-}
findWhereProperty : (a -> comparable) -> comparable -> List a -> Maybe a
findWhereProperty property propertyValue list =
  List.head (whereProperty property propertyValue list)

{-| Returns the list of the elements of the list that do not satisfy the given predicate.

....reject (\x -> x > 1) [1, 2, 3] == [1]
-}
reject : (a -> Bool) -> List a -> List a
reject predicate list = filter (\item -> not (predicate(item))) list

{-| Determine if all the elements in the list satisfy the predicate.

....every (\x -> x > 1) [1, 2, 3] == False
-}
every : (a -> Bool) -> List a -> Bool
every = List.all

{-| Determine if some of the elements in the list satisfy the predicate.

....every (\x -> x > 1) [1, 2, 3] == True
-}
some : (a -> Bool) -> List a -> Bool
some = List.any

{-| Determine if the list contains the element.

    contains 2 [1 2 3] == True
-}
contains : a -> List a -> Bool
contains = List.member

-- Skipping "invoke", no object methods in Elm

{-| Extract a list of dictionary values with a specific key.

....pluckDict "name" [
      Dict.fromList [("name", "name1"), ("email", "email1")],
      Dict.fromList [("email", "email2")],
      Dict.fromList [("name", "name3"), ("email", "email3")]
    ] == [Just "name1", Nothing, Just "name3"]
-}
pluckDict : comparable -> List (Dict comparable v) -> List (Maybe v)
pluckDict keyName list = List.map (\d -> (Dict.get keyName d)) list

{-| Extract a list of values from a list of records by a derived property.

....pluck .name [
      { name="Alice", height=1.62 }
      , { name="Bob", height=1.85 }
      , { name="Chuck", height=1.76 }
    ] == ["Alice", "Bob", "Chuck"]
-}
pluck : (a -> comparable) -> List a -> List comparable
pluck property list = List.map property list

{-| Return minimum element in the list.

....min [2, 1, 3] == 1
-}
min : List comparable -> Maybe comparable
min list = List.minimum list

{-| Return maximum element in the list.

....min [2, 1, 3] == 3
-}
max : List comparable -> Maybe comparable
max list = List.maximum list

{-| Sort list by a given property

....sortBy .name
      [
        {name = "Bob"},
        {name = "Steve"},
        {name = "Alice"}
      ] == [
             {name = "Alice"},
             {name = "Bob"},
             {name = "Steve"}
          ]
-}
sortBy : (a -> comparable) -> List a -> List a
sortBy property list = List.sortBy property list

{-| Groups elements of the list by a given property into a dictionary.

    groupBy
      (\x -> if x % 2 == 0 then "odd" else "even")
      [0, 1, 2, 3] == Dict.fromList [
                        ("even", [0, 2]),
                        ("odd", [1, 3])
                      ]
-}
groupBy : (a -> comparable) -> List a -> Dict comparable (List a)
groupBy property list =
  let
    appendToDictValue = (\list value ->
      case list of
        Just list -> Just (value :: list)
        Nothing -> Just [value])
    reductionStep = (\item dict ->
      let
        itemProperty = property item
      in
        (
          Dict.update
            itemProperty
            (\maybeValue -> (appendToDictValue maybeValue item))
            dict
        )
    )
  in 
    List.foldr reductionStep Dict.empty list

{-| Same as groupBy, but can be used for the case when we know that the given property is unique.

....indexBy
      (\x ->
        let
          maybeFirstSymbol = List.head <| String.toList x
        in case maybeFirstSymbol of
          Just firstSymbol -> (String.fromChar firstSymbol)
          Nothing -> "?")
      ["abc", "bca", "cab"]
      ==  Dict.fromList [
        ("a", "abc"),
        ("b", "bca"),
        ("c", "cab")
      ]
-}
indexBy : (a -> comparable) -> List a -> Dict comparable a
indexBy property list =
  let
    reductionStep = (\item dict ->
      let
        itemProperty = property item
      in
        (
          Dict.update
            itemProperty
            (\maybeValue -> (Just item))
            dict
        )
    )
  in 
    List.foldr reductionStep Dict.empty list

{-| Shuffles the given list using the Fisher and Yates'
  original method https://en.wikipedia.org/wiki/Fisher%E2%80%93Yates_shuffle
  if the same randomSeed is provided, the same shuffling is produced

    (shuffle [1, 2, 3, 4, 5, 6] (initialSeed 123)) == [4, 1, 5, 2, 3, 6])
-}
shuffle : List a -> Seed -> List a
shuffle list randomSeed = Array.toList (shuffleArr (Array.fromList list) randomSeed)

{-| Shuffles the given array using the Fisher and Yates'
  original method https://en.wikipedia.org/wiki/Fisher%E2%80%93Yates_shuffle
  if the same randomSeed is provided, the same shuffling is produced

    (shuffleArr (Array.fromList [1, 2, 3, 4, 5, 6]) (initialSeed 123)) == (Array.fromList [4, 1, 5, 2, 3, 6])
-}
shuffleArr : Array a -> Seed -> Array a
shuffleArr arr randomSeed = sampleArr arr (Array.length arr) randomSeed

{-| Samples list. If the same randomSeed it provided the same sampling is produced.

    (sample [1, 2, 3, 4, 5, 6] 3 (initialSeed 123)) == [2, 3, 6]
-}
sample : List a -> Int -> Seed -> List a
sample list sampleSize randomSeed = Array.toList (sampleArr (Array.fromList list) sampleSize randomSeed)

{-| Samples array. If the same randomSeed it provided the same sampling is produced.

    (sampleArr (Array.fromList [1, 2, 3, 4, 5, 6]) 3 (initialSeed 123)) == (Array.fromList [2, 3, 6])
-}
sampleArr : Array a -> Int -> Seed -> Array a
sampleArr arr sampleSize randomSeed =
  let
    (randomIndex, nextRandomSeed) = step (Random.int 0 ((Array.length arr) - 1)) randomSeed
    maybeRandomElement = Array.get randomIndex arr
    arrayBeforeRandomElement = Array.slice 0 randomIndex arr 
    arrayAfterRandomElement = Array.slice (randomIndex + 1) (Array.length arr) arr
  in
    case maybeRandomElement of
      Just randomElement ->
        (case sampleSize of
          1 -> Array.fromList [randomElement]
          x -> if x > 1 then
              push randomElement (sampleArr (append arrayBeforeRandomElement arrayAfterRandomElement) (sampleSize - 1) nextRandomSeed)
            else
              Array.fromList [])
      Nothing -> fromList [] -- this branch should never be executed as maybeRandomElement always should be Just x

{-| Same as sample with sampleSize of 1 but returns a single element, not a list.

    (sampleOne [1, 2, 3, 4, 5, 6] (initialSeed 123)) == Just 6
-}
sampleOne : List a -> Seed -> Maybe a
sampleOne list randomSeed = sampleArrOne (Array.fromList list) randomSeed

{-| Same as sample with sampleSize of 1 but returns a single element, not an array.

    (sampleArrOne (Array.fromList [1, 2, 3, 4, 5, 6]) (initialSeed 123)) == Just 6
-}
sampleArrOne : Array a -> Seed -> Maybe a
sampleArrOne arr randomSeed = Array.get 0 (sampleArr arr 1 randomSeed)

{-| Convert list to array. Alias for 'Array.fromList'

....(toArray [1, 2, 3]) == (Array.fromList [1, 2, 3])
-}
toArray : List a -> Array a
toArray = Array.fromList

{-| Returns length of the given list. Alias for 'List.length'

    (size [1, 2, 3, 4, 5]) == 5
-}
size : List a -> Int
size = List.length

{-| Partitions list into two parts: one contains all the elements that satisfy the predicate and those that does not satisfy it.

....(partition (\x -> x % 2 == 0) [1, 2, 3, 4, 5]) == ([2, 4], [1, 3, 5])
-}
partition : (a -> Bool) -> List a -> (List a, List a)
partition predicate list =
  let
    first = filter predicate list
    second = filter (\x -> not (predicate x) ) list
  in
    (first, second)

{-| Returns first n elements of the list.

....(first 3 [1 2 3 4 5]) == [1 2 3]
-}
first : Int -> List a -> List a
first = List.take

{-| Returns first n elements of the array.

....(first 3 (Array.fromList [1 2 3 4 5]) ) == (Array.fromList [1 2 3])
-}
firstArr : Int -> Array a -> Array a
firstArr n = Array.slice 0 n

{-| Returns first element of the list.

....(firstOne [1, 2, 3]) == (Maybe 1)
-}
firstOne : List a -> Maybe a
firstOne = List.head

{-| Returns first element of the array.

....(firstArrOne (toArray [1, 2, 3]) ) == (Maybe 1)
-}
firstArrOne : Array a -> Maybe a
firstArrOne = Array.get(0)

{-| Return everything but the last n entries of the list.

    (initial 2 [1, 2, 3, 4, 5]) == [1, 2, 3]
-}
initialArr : Int -> Array a -> Array a
initialArr n arr = 
  if (n > 0) then
    Array.slice 0 -n arr
  else
    arr

{-| Shortcut for (initial 1)

....(initialOne [1, 2, 3]) == [1, 2]
-}
initialArrOne : Array a -> Array a
initialArrOne = initialArr 1

{-| Take last n entries of the list

....(last 3 (Array.fromList [1, 2, 3, 4, 5]) ) == [3, 4, 5]
-}
lastArr : Int -> Array a -> Array a
lastArr n arr =
  let
    len = Array.length arr
  in
    if (n < len) then
      Array.slice (len - n) len arr
    else
      arr

{-| Take last entry in the list.

    (last (Array.fromList [1, 2, 3]) ) == Maybe 3
-}
lastArrOne : Array a -> Maybe a
lastArrOne arr =
  if Array.isEmpty arr then
    Nothing
  else
    Array.get (Array.length arr - 1) arr