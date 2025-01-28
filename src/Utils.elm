module Utils exposing (classList, ifElse, onlyIf)

import Html exposing (Attribute)
import Html.Attributes exposing (class)


classList : List String -> Attribute msg
classList =
    String.join " " >> class


ifElse : Bool -> a -> a -> a
ifElse c x y =
    if c then
        x

    else
        y


onlyIf : Bool -> List a -> List a
onlyIf c x =
    ifElse c x []
