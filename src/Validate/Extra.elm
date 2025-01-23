module Validate.Extra exposing (..)


import Validate as V
import Set exposing (Set)
import Set


ifLongerThan : (subject -> String) -> Int -> error -> V.Validator error subject
ifLongerThan f n = V.ifTrue (f >> String.length >> (<=) n)


ifNotUnique : (subject -> comparable) -> Set comparable -> error -> V.Validator error subject
ifNotUnique f inSet = V.ifTrue (f >> \v -> Set.member v inSet)


skip : V.Validator error subject
skip = V.all []


isValidPhone : String -> Bool
isValidPhone = always True -- FIXME