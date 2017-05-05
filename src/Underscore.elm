module Underscore exposing (map, reduce, reduceRight)

{-| Port to Elm of Underscore 1.8.3 functions.

@docs map
@docs reduce
@docs reduceRight
-}

import List exposing (map, foldl)

{-| Transforms a given list by applying the provided element transformation function to every element.

    map [1, 2, 3] (\x -> x > 1) == [False, True, True]
-}
map : (a -> b) -> List a -> List b
map = List.map

{-| Reduces the list given the initial value and the reduction step definition.

    reduce (\s x -> s + x) 0 [1, 2, 3] == 6
-}
reduce : (a -> b -> b) -> b -> List a -> b
reduce = List.foldl

{-| Reduces the list given the initial value and the reduction step definition.

    reduce (\s x -> s + x) 0 [1, 2, 3] == 6
-}
reduceRight : (a -> b -> b) -> b -> List a -> b
reduceRight = List.foldr