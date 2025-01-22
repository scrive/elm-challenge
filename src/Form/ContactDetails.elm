module Form.ContactDetails exposing (..)


import Html exposing (Html)
import Html

import Maybe

import Scrive.ContactDetails exposing (ContactDetails)
import Scrive.ContactDetails as CD


view : ContactDetails -> Html msg
view { address } =
    Html.ul
        []
        [ Html.li [] [ Html.text <| "e-mail : " ++ (Maybe.withDefault "-" <| Maybe.map CD.emailToString address.email) ]
        , Html.li [] [ Html.text <| "phone : " ++ (Maybe.withDefault "-" <| Maybe.map CD.phoneToString address.phone) ]
        , Html.li [] [ Html.text <| "company : " ++ (Maybe.withDefault "-" address.companyName) ]
        , Html.li [] [ Html.text <| "address : " ++ (Maybe.withDefault "-" address.address) ]
        , Html.li [] [ Html.text <| "zip : " ++ (Maybe.withDefault "-" <| Maybe.map CD.zipCodeToString address.zip) ]
        , Html.li [] [ Html.text <| "city : " ++ (Maybe.withDefault "-" address.city) ]
        , Html.li [] [ Html.text <| "country : " ++ (Maybe.withDefault "-" address.country) ]
        ]