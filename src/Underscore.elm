module Underscore exposing (map)

map : List a -> (a -> b) -> List b
map list f =
  case list of
    [] -> []
    head::tail -> (f head)::(map tail f)