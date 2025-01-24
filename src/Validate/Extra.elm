module Validate.Extra exposing (..)


import Validate as V

import Char
import Regex exposing (Regex)
import Set exposing (Set)
import Set


ifLongerThan : (subject -> String) -> Int -> error -> V.Validator error subject
ifLongerThan f n = V.ifTrue (f >> String.length >> (<=) n)


ifNotUnique : (subject -> comparable) -> Set comparable -> error -> V.Validator error subject
ifNotUnique f inSet = V.ifTrue (f >> \v -> Set.member v inSet)


ifNotAllDigits : (subject -> String) -> error ->  V.Validator error subject
ifNotAllDigits f = V.ifFalse (f >> String.all Char.isDigit)


ifNotAllAlphaNum : (subject -> String) -> error ->  V.Validator error subject
ifNotAllAlphaNum f = V.ifFalse (f >> String.all Char.isAlphaNum)


skip : V.Validator error subject
skip = V.all []


isValidPhone : String -> Bool
isValidPhone = Regex.contains validPhone


validPhone : Regex
validPhone =
    -- original: /^\+?\d{1,4}?[-.\s]?\(?\d{1,3}?\)?[-.\s]?\d{1,4}[-.\s]?\d{1,4}[-.\s]?\d{1,9}$/
    "^\\+?\\d{1,4}?[-.\\s]?\\(?\\d{1,3}?\\)?[-.\\s]?\\d{1,4}[-.\\s]?\\d{1,4}[-.\\s]?\\d{1,9}$"
        |> Regex.fromStringWith { caseInsensitive = True, multiline = False }
        |> Maybe.withDefault Regex.never


-- Zip Codes format from all over the world: https://gist.github.com/jamesbar2/1c677c22df8f21e869cca7e439fc3f5b