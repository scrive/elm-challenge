module Data.Field exposing (Field(..), Option, TextType(..), getInputType)


type Field
    = Text TextType
    | Radio (List Option)
    | Checkbox


type alias Option =
    ( String, String )


type TextType
    = Plain
    | Email
    | Phone


getInputType : Field -> String
getInputType field =
    case field of
        Text textType ->
            case textType of
                Email ->
                    "email"

                Phone ->
                    "tel"

                Plain ->
                    ""

        Radio _ ->
            "radio"

        Checkbox ->
            "checkbox"
