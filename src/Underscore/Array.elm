module Underscore.Array exposing (
  shuffle,
  sample,
  sampleOne,
  first,
  firstOne,
  initial,
  initialOne,
  last,
  lastOne)

{-| Port to Elm of Underscore 1.8.3 functions.

@docs shuffle
@docs sample
@docs sampleOne
@docs first
@docs firstOne
@docs initial
@docs initialOne
@docs last
@docs lastOne
-}

import Array exposing (Array, fromList, append, get, slice, length, push)
import Random exposing (Seed, int, step)

{-| Shuffles the given array using the Fisher and Yates'
  original method https://en.wikipedia.org/wiki/Fisher%E2%80%93Yates_shuffle
  if the same randomSeed is provided, the same shuffling is produced

    (shuffle (Array.fromList [1, 2, 3, 4, 5, 6]) (initialSeed 123)) == (Array.fromList [4, 1, 5, 2, 3, 6])
-}
shuffle : Array a -> Seed -> Array a
shuffle arr randomSeed = sample arr (Array.length arr) randomSeed

{-| Samples array. If the same randomSeed it provided the same sampling is produced.

    (sample (Array.fromList [1, 2, 3, 4, 5, 6]) 3 (initialSeed 123)) == (Array.fromList [2, 3, 6])
-}
sample : Array a -> Int -> Seed -> Array a
sample arr sampleSize randomSeed =
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
              push randomElement (sample (append arrayBeforeRandomElement arrayAfterRandomElement) (sampleSize - 1) nextRandomSeed)
            else
              Array.fromList [])
      Nothing -> fromList [] -- this branch should never be executed as maybeRandomElement always should be Just x

{-| Same as sample with sampleSize of 1 but returns a single element, not an array.

    (sampleOne (Array.fromList [1, 2, 3, 4, 5, 6]) (initialSeed 123)) == Just 6
-}
sampleOne : Array a -> Seed -> Maybe a
sampleOne arr randomSeed = Array.get 0 (sample arr 1 randomSeed)

{-| Returns first n elements of the array.

....(first 3 (Array.fromList [1 2 3 4 5]) ) == (Array.fromList [1 2 3])
-}
first : Int -> Array a -> Array a
first n = Array.slice 0 n

{-| Returns first element of the array.

....(firstArrOne (toArray [1, 2, 3]) ) == (Maybe 1)
-}
firstOne : Array a -> Maybe a
firstOne = Array.get(0)

{-| Return everything but the last n entries of the array.

    (initial 2 [1, 2, 3, 4, 5]) == [1, 2, 3]
-}
initial : Int -> Array a -> Array a
initial n arr = 
  if (n > 0) then
    Array.slice 0 -n arr
  else
    arr

{-| Shortcut for (initial 1)

....(initialOne [1, 2, 3]) == [1, 2]
-}
initialOne : Array a -> Array a
initialOne = initial 1

{-| Take last n entries of the list

....(last 3 (Array.fromList [1, 2, 3, 4, 5]) ) == [3, 4, 5]
-}
last : Int -> Array a -> Array a
last n arr =
  let
    len = Array.length arr
  in
    if (n < len) then
      Array.slice (len - n) len arr
    else
      arr

{-| Take last entry in the array.

    (last (Array.fromList [1, 2, 3]) ) == Maybe 3
-}
lastOne : Array a -> Maybe a
lastOne arr =
  if Array.isEmpty arr then
    Nothing
  else
    Array.get (Array.length arr - 1) arr