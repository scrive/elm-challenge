module Data.Answer exposing
    ( Answer(..)
    , AnswerValidator(..)
    , isEmpty
    , toString
    , toValidators
    )

import Regex
import Validate exposing (Validator)


type Answer
    = Text String


toString : Answer -> String
toString answer =
    case answer of
        Text str ->
            str


isEmpty : Answer -> Bool
isEmpty =
    toString >> String.isEmpty


type AnswerValidator
    = ValidEmail
    | ValidPhone
    | MinLength Int
    | EmptyInput


toValidators : AnswerValidator -> List (Validator String Answer)
toValidators answerValidator =
    case answerValidator of
        EmptyInput ->
            [ Validate.ifTrue ifEmpty emptyInputError ]

        ValidEmail ->
            [ Validate.ifInvalidEmail toString (always invalidEmailError) ]

        ValidPhone ->
            [ Validate.ifFalse (toString >> ifValidPhone) invalidPhoneError ]

        MinLength length ->
            [ Validate.ifFalse (toString >> hasMinLength length) invalidLengthError ]


ifEmpty : Answer -> Bool
ifEmpty subject =
    case subject of
        Text value ->
            value == ""


ifValidPhone : String -> Bool
ifValidPhone =
    Regex.fromString "^[0-9+()\\s]*$"
        |> Maybe.withDefault Regex.never
        |> Regex.contains


hasMinLength : Int -> String -> Bool
hasMinLength minLength str =
    String.length str >= minLength


emptyInputError : String
emptyInputError =
    "Please provide a value."


invalidEmailError : String
invalidEmailError =
    "Please provide a valid e-mail address."


invalidPhoneError : String
invalidPhoneError =
    "Please provide a valid phone number."


invalidLengthError : String
invalidLengthError =
    "The input value is too short. Please provide a valid value."
