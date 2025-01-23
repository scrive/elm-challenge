module Utils exposing (classList)

import Html exposing (Attribute)
import Html.Attributes exposing (class)


classList : List String -> Attribute msg
classList =
    String.join " " >> class
