module Form.RetentionPolicy exposing (..)


import Html exposing (Html)
import Html

import Scrive.Settings exposing (PolicyList)
import Scrive.Settings as S


view : PolicyList -> Html msg
view =
    Html.ul
        []
        << List.map (\{ policy, value } -> Html.li [] [ Html.text <| S.retentionPolicyToString policy ++ " timeout : " ++ String.fromInt value ])
