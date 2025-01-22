module Ui.Form exposing (view)

import Html exposing (Html)
import Html.Attributes as Attributes


baseCssClasses : String
baseCssClasses =
    "max-w-sm mx-auto"


view : List (Html msg) -> Html msg
view =
    Html.form [ Attributes.class baseCssClasses ]
