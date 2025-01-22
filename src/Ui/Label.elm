module Ui.Label exposing (view)

import Html exposing (Html)
import Html.Attributes as Attributes


type alias Model =
    { text : String
    , inputId : String
    }


baseCssClasses : String
baseCssClasses =
    "block mb-2 text-sm font-medium text-gray-900 dark:text-white"


view : Model -> Html msg
view { inputId, text } =
    Html.label
        [ Attributes.class baseCssClasses
        , Attributes.for inputId
        ]
        [ Html.text text ]
