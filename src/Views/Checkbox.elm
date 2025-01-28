module Views.Checkbox exposing (view)

import Data.Answer as Answer
import Data.Field exposing (getInputType)
import Data.Form exposing (Entry, hasErrors)
import Data.Question exposing (Question, tagToString)
import Html exposing (Attribute, Html, div, input, label, text)
import Html.Attributes exposing (checked, disabled, for, id, type_)
import Html.Events exposing (onClick)
import Msg exposing (Msg(..))
import Utils exposing (classList, ifElse, onlyIf)


type alias Props =
    { disabled : Bool }


view : Props -> Question -> Entry -> Html Msg
view props question entry =
    div [ stylesCheckbox ]
        [ viewLabel question entry
        , viewInput props question entry
        ]


viewLabel : Question -> Entry -> Html Msg
viewLabel question entry =
    label
        [ stylesLabel <| hasErrors <| Tuple.second entry
        , for <| tagToString question.tag
        ]
        [ text question.title ]


viewInput : Props -> Question -> Entry -> Html Msg
viewInput props question ( answer, validation ) =
    input
        [ type_ <| getInputType question.field
        , stylesInput <| hasErrors validation
        , onClick <| (ifElse (Answer.isEmpty answer) "checked" "" |> Answer.Text |> AnswerQuestion question)
        , id <| tagToString question.tag
        , checked <| (answer |> Answer.isEmpty |> not)
        , disabled props.disabled
        ]
        []


stylesCheckbox : Attribute Msg
stylesCheckbox =
    classList
        [ "flex"
        , "gap-2"
        , "items-center"
        ]


stylesLabel : Bool -> Attribute Msg
stylesLabel hasErrors_ =
    classList <|
        [ "text-sm"
        , "font-medium"
        , "text-gray-900"
        ]
            ++ onlyIf hasErrors_
                [ "text-red-900" ]


stylesInput : Bool -> Attribute Msg
stylesInput hasErrors_ =
    classList <|
        [ "w-4"
        , "h-4"
        , "text-blue-600"
        , "bg-gray-100"
        , "border-gray-300"
        , "rounded-sm"
        , "focus:ring-blue-500"
        , "focus:ring-2"
        , "cursor-pointer"
        ]
            ++ onlyIf hasErrors_
                [ "border-red-500"
                , "text-red-900"
                ]
