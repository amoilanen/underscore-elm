module Underscore exposing (map, reduce, reduceRight, find)

{-| Port to Elm of Underscore 1.8.3 functions.

@docs map
@docs reduce
@docs reduceRight
@docs find
-}

import List exposing (map, foldl)

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
  case list of
    head::rest ->
      if predicate head then
        Just head
      else
        find predicate rest
    [] -> Nothing
