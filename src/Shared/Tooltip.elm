module Shared.Tooltip exposing (view)

import Html exposing (Html)
import Html.Attributes as Attrs


view : String -> String -> Html msg
view id tooltipText =
    Html.div
        [ Attrs.id id
        , Attrs.class "absolute bottom-full mb-2 left-1/2 transform -translate-x-1/2 px-2 py-1 bg-gray-800 text-white text-xs rounded opacity-0 pointer-events-none transition-opacity duration-200 group-hover:opacity-100 group-focus:opacity-100 z-10 whitespace-nowrap"
        , Attrs.attribute "role" "tooltip"
        ]
        [ Html.text tooltipText ]
