module Views.Question exposing (view)

import Data.Field exposing (Field(..), TextType(..))
import Data.Form exposing (Entry, hasErrors)
import Data.Question exposing (Question, tagToString)
import Html exposing (Attribute, Html, div, label, text)
import Html.Attributes exposing (for)
import Html.Extra exposing (viewIf)
import Msg exposing (Msg(..))
import Utils exposing (classList, onlyIf)
import Views.Checkbox as Checkbox
import Views.Errors as Errors
import Views.Radio as Radio
import Views.TextField as TextField


type alias Props =
    { disabled : Bool }


view : Props -> Question -> Entry -> Html Msg
view props question entry =
    div [ stylesQuestion ]
        [ viewIf (question.field /= Checkbox)
            (viewLabel question entry)
        , viewField props question entry
        , Errors.view <| Tuple.second entry
        ]


viewLabel : Question -> Entry -> Html Msg
viewLabel question entry =
    label
        [ stylesLabel <| hasErrors <| Tuple.second entry
        , for <| tagToString question.tag
        ]
        [ text question.title ]


viewField : Props -> Question -> Entry -> Html Msg
viewField props question entry =
    case question.field of
        Text _ ->
            TextField.view props question entry

        Radio options ->
            Radio.view props question entry options

        Checkbox ->
            Checkbox.view props question entry



-- Styles


stylesQuestion : Attribute Msg
stylesQuestion =
    classList
        [ "grid"
        , "gap-2"
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
