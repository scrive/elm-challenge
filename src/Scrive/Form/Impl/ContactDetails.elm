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


view : List Error -> { readOnly : Bool, toMsg : CD.Address -> msg } -> ContactDetails -> Html msg
view errors { readOnly, toMsg } { address } =
    if readOnly then
        Html.ul
            []
            [ Html.li [] [ Html.text <| "Preferred way of contact:" ++ CD.preferredContactToString address.preferredContactMethod ]
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

            inputFor : Field -> String -> String -> (CD.Address -> String) -> (String -> CD.Address) -> Html msg
            inputFor field labelText inputId fGetPart fSetPart =
                Html.li []
                    [ Html.label [ Attrs.for inputId ] [ Html.text <| labelText ++ " : " ]
                    , Html.input
                        [ Attrs.id inputId
                        , Attrs.type_ "text"
                        , Evts.onInput (fSetPart >> toMsg)
                        , Attrs.placeholder <| fGetPart address
                        -- , Evts.onEnter (fSetPart >> toMsg)
                        -- , Evts.onMouseOut (fSetPart >> toMsg)
                        ]
                        [ ]
                    , Errors.viewMany <| Errors.extractOnlyAt field errors
                    ]
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
            , inputFor Field.AddressEmail "E-mail" "contact-email"
                (.email >> Maybe.map CD.emailToString >> Maybe.withDefault "")
                (\nextEmail -> { address | email = Just <| CD.Email nextEmail })
            , inputFor Field.AddressPhone "Phone" "contact-phone"
                (.phone >> Maybe.map CD.phoneToString >> Maybe.withDefault "")
                (\nextPhone -> { address | phone = Just <| CD.Phone nextPhone })
            , inputFor Field.AddressCompanyName "Company name" "contact-company"
                (.companyName >> Maybe.withDefault "")
                (\nextCompany -> { address | companyName = Just nextCompany })
            , inputFor Field.AddressStreet "Street address" "contact-street-address"
                (.address >> Maybe.withDefault "")
                (\nextAddress -> { address | address = Just nextAddress })
            , inputFor Field.AddressZip "ZIP Code" "contact-zip-code"
                (.zip >> Maybe.map CD.zipCodeToString >> Maybe.withDefault "")
                (\nextZip -> { address | zip = Just <| CD.ZipCode nextZip })
            , inputFor Field.AddressCity "City" "contact-city"
                (.city >> Maybe.withDefault "")
                (\nextCity -> { address | city = Just nextCity })
            , inputFor Field.AddressCountry "Country" "contact-country"
                (.country >> Maybe.withDefault "")
                (\nextCountry -> { address | city = Just nextCountry })
            ]