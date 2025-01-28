module Views.TextField exposing (view)

import Data.Answer as Answer
import Data.Field exposing (Field(..), TextType(..), getInputType)
import Data.Form exposing (Entry, hasErrors)
import Data.Question exposing (Question, tagToString)
import Html exposing (Attribute, Html, input)
import Html.Attributes exposing (disabled, id, placeholder, type_, value)
import Html.Events.Extra exposing (onChange)
import Msg exposing (Msg(..))
import Utils exposing (classList, onlyIf)


type alias Props =
    { disabled : Bool }


view : Props -> Question -> Entry -> Html Msg
view props question ( answer, validation ) =
    input
        [ stylesInput <| hasErrors validation
        , placeholder question.placeholder
        , value <| Answer.toString answer
        , onChange <| Answer.Text >> AnswerQuestion question
        , id <| tagToString question.tag
        , type_ <| getInputType question.field
        , disabled props.disabled
        ]
        []



-- Styles


stylesInput : Bool -> Attribute Msg
stylesInput hasErrors_ =
    classList <|
        [ "bg-gray-50"
        , "border"
        , "border-gray-300"
        , "text-gray-900"
        , "text-sm"
        , "rounded-lg"
        , "focus:ring-blue-500"
        , "focus:border-blue-500"
        , "w-full"
        , "p-2.5"
        ]
            ++ onlyIf hasErrors_
                [ "border-red-500"
                , "text-red-900"
                ]
