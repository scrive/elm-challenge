module Scrive.Form.Impl.ContactDetails exposing (..)


import Html exposing (Html)
import Html
import Html.Extra as Html
import Html.Attributes as Attrs
import Html.Events as Evts
import Html.Events.Extra as Evts

import Maybe

import Scrive.Data.ContactDetails exposing (ContactDetails)
import Scrive.Data.Address as CD
import Scrive.Form.Field exposing (Field)
import Scrive.Form.Field as Field
import Scrive.Form.Error exposing (Error)
import Scrive.Form.Error as Errors


type alias Handlers msg =
    { toMsg : CD.Address -> msg
    , editField : ( CD.Field, String ) -> msg
    }


type alias State =
    { readOnly : Bool, currentlyEditing : Maybe ( CD.Field, String ) }


view : List Error -> Handlers msg -> State -> ContactDetails -> Html msg
view errors { toMsg, editField } { readOnly, currentlyEditing } { address } =
    if readOnly then
        Html.ul
            []
            [ Html.li [] [ Html.text <| "Preferred way of contact :" ++ CD.preferredContactToString address.preferredContactMethod ]
            , Html.li [] [ Html.text <| "E-mail : " ++ (Maybe.withDefault "-" <| Maybe.map CD.emailToString address.email) ]
            , Html.li [] [ Html.text <| "Phone : " ++ (Maybe.withDefault "-" <| Maybe.map CD.phoneToString address.phone) ]
            , Html.li [] [ Html.text <| "Company name : " ++ (Maybe.withDefault "-" address.companyName) ]
            , Html.li [] [ Html.text <| "Steet address : " ++ (Maybe.withDefault "-" address.address) ]
            , Html.li [] [ Html.text <| "ZIP Code : " ++ (Maybe.withDefault "-" <| Maybe.map CD.zipCodeToString address.zip) ]
            , Html.li [] [ Html.text <| "City : " ++ (Maybe.withDefault "-" address.city) ]
            , Html.li [] [ Html.text <| "Country : " ++ (Maybe.withDefault "-" address.country) ]
            ]
    else
        let

            preferredContactOption : CD.PreferredContact -> Html msg
            preferredContactOption pc =
                Html.option
                    [ Attrs.selected <| pc == address.preferredContactMethod ]
                    [ Html.text <| CD.preferredContactToString pc ]

            inputFor field inputId fGetPart fSetPart currentValue =
                Html.li []
                        [ Html.label [ Attrs.for inputId ] [ Html.text <| CD.fieldToLabel field ++ " : " ]
                        , Html.input
                            [ Attrs.id inputId
                            , Attrs.type_ "text"
                            , Evts.onInput (\str -> editField ( field, str ))
                            , Attrs.placeholder <| fGetPart address
                            , Attrs.value currentValue
                            -- , Evts.onEnter (fSetPart >> toMsg)
                            -- , Evts.onMouseOut (fSetPart >> toMsg)
                            ]
                            [ ]
                        , Html.button
                            [ Evts.onClick (fSetPart currentValue |> toMsg) ]
                            [ Html.text "(Set)" ]
                        , Errors.viewMany <| Errors.extractOnlyAt (Field.Address field) errors
                        ]

            clickableTextFor field fGetPart =
                Html.li
                    [ Evts.onClick <| editField ( field, fGetPart address ) ]
                    [ Html.text <| CD.fieldToLabel field ++ " : " ++ fGetPart address
                    , Errors.viewMany <| Errors.extractOnlyAt (Field.Address field) errors
                    ]

            inputOrTextFor : CD.Field -> String -> (CD.Address -> String) -> (String -> CD.Address) -> Html msg
            inputOrTextFor field inputId fGetPart fSetPart =
                case currentlyEditing of
                    Just ( currentField, currentValue ) ->
                        if currentField == field then
                            inputFor field inputId fGetPart fSetPart currentValue
                        else
                            clickableTextFor field fGetPart
                    Nothing ->
                        clickableTextFor field fGetPart

        in
        Html.ul
            []
            [ Html.li []
                [ Html.label  [ Attrs.for "preferred-contact" ] [ Html.text "Preferred way of contact:" ]
                , Html.select
                    [ Attrs.id "preferred-contact"
                    , Evts.onInput <| \str -> toMsg { address | preferredContactMethod = CD.preferredContactFromOption str }
                     ] <| List.map preferredContactOption CD.preferredWaysToContact
                ]
            , inputOrTextFor CD.F_Email "contact-email"
                (.email >> Maybe.map CD.emailToString >> Maybe.withDefault "")
                (\nextEmail -> { address | email = Just <| CD.Email nextEmail })
            , inputOrTextFor CD.F_Phone "contact-phone"
                (.phone >> Maybe.map CD.phoneToString >> Maybe.withDefault "")
                (\nextPhone -> { address | phone = Just <| CD.Phone nextPhone })
            , inputOrTextFor CD.F_CompanyName "contact-company"
                (.companyName >> Maybe.withDefault "")
                (\nextCompany -> { address | companyName = Just nextCompany })
            , inputOrTextFor CD.F_StreetAddress "contact-street-address"
                (.address >> Maybe.withDefault "")
                (\nextAddress -> { address | address = Just nextAddress })
            , inputOrTextFor CD.F_ZipCode "contact-zip-code"
                (.zip >> Maybe.map CD.zipCodeToString >> Maybe.withDefault "")
                (\nextZip -> { address | zip = Just <| CD.ZipCode nextZip })
            , inputOrTextFor CD.F_City "contact-city"
                (.city >> Maybe.withDefault "")
                (\nextCity -> { address | city = Just nextCity })
            , inputOrTextFor CD.F_Country "contact-country"
                (.country >> Maybe.withDefault "")
                (\nextCountry -> { address | country = Just nextCountry })
            ]