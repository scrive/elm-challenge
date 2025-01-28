module Views.Radio exposing (view)

import Data.Answer as Answer
import Data.Field exposing (Field(..), Option, TextType(..), getInputType)
import Data.Form exposing (Entry, hasErrors)
import Data.Question exposing (Question, tagToString)
import Html exposing (Attribute, Html, div, input, label, text)
import Html.Attributes exposing (checked, disabled, for, id, type_, value)
import Html.Events exposing (onClick)
import Msg exposing (Msg(..))
import Utils exposing (classList, onlyIf)


type alias Props =
    { disabled : Bool }


view : Props -> Question -> Entry -> List Option -> Html Msg
view props question entry options =
    div [ stylesRadioGroup ] <|
        List.map (viewRadio props question entry) options


viewRadio : Props -> Question -> Entry -> Option -> Html Msg
viewRadio props question entry option =
    div [ stylesRadio ]
        [ viewLabel question entry option
        , viewInput props question entry option
        ]


viewLabel : Question -> Entry -> Option -> Html Msg
viewLabel question entry ( _, label_ ) =
    label
        [ stylesLabel <| hasErrors <| Tuple.second entry
        , for <| tagToString question.tag
        ]
        [ text label_ ]


viewInput : Props -> Question -> Entry -> Option -> Html Msg
viewInput props question ( answer, validation ) ( value_, _ ) =
    input
        [ type_ <| getInputType question.field
        , stylesInput <| hasErrors validation
        , value value_
        , onClick <| (value_ |> Answer.Text |> AnswerQuestion question)
        , id <| tagToString question.tag
        , checked <| value_ == Answer.toString answer
        , disabled props.disabled
        ]
        []



-- Styles


stylesRadioGroup : Attribute Msg
stylesRadioGroup =
    classList
        [ "grid"
        , "gap-2"
        ]


stylesRadio : Attribute Msg
stylesRadio =
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
                [ "text-red-700" ]


stylesInput : Bool -> Attribute Msg
stylesInput hasErrors_ =
    classList <|
        [ "w-4"
        , "h-4"
        , "text-blue-600"
        , "bg-gray-100"
        , "border-gray-300"
        , "focus:ring-blue-500"
        , "focus:ring-2"
        , "cursor-pointer"
        ]
            ++ onlyIf hasErrors_
                [ "border-red-500"
                , "text-red-900"
                ]
