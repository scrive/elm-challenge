module Errors exposing (errorString)

import Form.Error exposing (..)


errorString : ErrorValue () -> String
errorString error =
    case error of
        Empty ->
            "The field can not be empty."

        InvalidString ->
            "This field is required."

        InvalidEmail ->
            "That is not a valid email address."

        InvalidFormat ->
            "That is not the correct format."

        InvalidInt ->
            "That is not a valid number."

        InvalidFloat ->
            "That is not a valid decimal number."

        InvalidBool ->
            "That is not a valid option."

        SmallerIntThan n ->
            "Can not be smaller than " ++ String.fromInt n ++ "."

        GreaterIntThan n ->
            "Can not be greater than " ++ String.fromInt n ++ "."

        SmallerFloatThan n ->
            "Can not be smaller than " ++ String.fromFloat n ++ "."

        GreaterFloatThan n ->
            "Can not be greater than " ++ String.fromFloat n ++ "."

        ShorterStringThan n ->
            "Must be at least " ++ String.fromInt n ++ " characters long."

        LongerStringThan n ->
            "Can not be more than " ++ String.fromInt n ++ " characters long."

        NotIncludedIn ->
            "Is not a valid selection from the list."

        CustomError _ ->
            "Custom error"
