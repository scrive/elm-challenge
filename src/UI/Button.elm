module UI.Button exposing (view)

import Html exposing (Attribute, Html, button, text)
import Msg exposing (Msg)
import Utils exposing (classList)


type alias Props =
    { label : String }


view : Props -> List (Attribute Msg) -> Html Msg
view props attributes =
    button (stylesButton :: attributes) [ text props.label ]



-- Styles


stylesButton : Attribute Msg
stylesButton =
    classList
        [ "text-white"
        , "bg-blue-700"
        , "hover:bg-blue-800"
        , "focus:ring-4"
        , "focus:ring-blue-300"
        , "font-medium"
        , "rounded-lg"
        , "text-sm"
        , "px-5"
        , "py-2.5"
        , "me-2"
        , "mb-2"
        ]
