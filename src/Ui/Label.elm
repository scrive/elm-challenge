module Ui.Label exposing (view)

import Html exposing (Html)
import Html.Attributes as Attributes


type alias Model =
    { text : String
    , htmlFor : String
    }


baseCssClasses : String
baseCssClasses =
    "block mb-2 text-sm font-medium text-gray-900 dark:text-white"


view : Model -> Html msg
view { htmlFor, text } =
    Html.label
        [ Attributes.class baseCssClasses
        , Attributes.for htmlFor
        ]
        [ Html.text text ]
