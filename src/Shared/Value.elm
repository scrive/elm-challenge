module Shared.Value exposing (Value, addError, fromValue, getMaybeError, isInvalid, isValid, validValue)


type Value a
    = Valid a
    | Invalid String a


validValue : a -> Value a
validValue =
    Valid


fromValue : Value a -> a
fromValue value =
    case value of
        Valid a ->
            a

        Invalid _ a ->
            a


getMaybeError : Value a -> Maybe String
getMaybeError value =
    case value of
        Invalid error _ ->
            Just error

        Valid _ ->
            Nothing


isValid : Value a -> Bool
isValid value =
    case value of
        Invalid _ _ ->
            False

        Valid _ ->
            True


isInvalid : Value a -> Bool
isInvalid =
    not << isValid


addError : String -> Value a -> Value a
addError error value =
    Invalid error <| fromValue value
