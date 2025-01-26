module Ui.Form exposing (view)

import Html exposing (Html)
import Html.Events as Events


view : msg -> List (Html msg) -> Html msg
view submitMsg =
    Html.form [ Events.onSubmit submitMsg ]
