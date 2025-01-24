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
    { setContactMethod : CD.PreferredContact -> msg
    , tryUpdate : CD.DraftAddress -> msg
    , editField : ( CD.Field, String ) -> msg
    }


type alias State =
    { readOnly : Bool, currentlyEditing : Maybe ( CD.Field, String ) }


view : List Error -> Handlers msg -> State -> ContactDetails -> Html msg
view errors { setContactMethod, tryUpdate, editField } { readOnly, currentlyEditing } { address } =
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

            draftAddress = CD.toDraft address

            qValueOf : CD.Field -> String
            qValueOf =  Maybe.withDefault "" << CD.draftValueOf draftAddress

            preferredContactOption : CD.PreferredContact -> Html msg
            preferredContactOption pc =
                Html.option
                    [ Attrs.selected <| pc == address.preferredContactMethod ]
                    [ Html.text <| CD.preferredContactToString pc ]

            inputFor field inputId currentValue =
                Html.li []
                        [ Html.label [ Attrs.for inputId ] [ Html.text <| CD.fieldToLabel field ++ " : " ]
                        , Html.input
                            [ Attrs.id inputId
                            , Attrs.type_ "text"
                            , Evts.onInput (\str -> editField ( field, str ))
                            , Attrs.placeholder <| qValueOf field
                            , Attrs.value currentValue
                            -- , Evts.onEnter (fSetPart >> toMsg)
                            -- , Evts.onMouseOut (fSetPart >> toMsg)
                            ]
                            [ ]
                        , Html.button
                            [ Evts.onClick (CD.setDraftValue draftAddress field currentValue |> tryUpdate) ]
                            [ Html.text "(Set)" ]
                        , Errors.viewMany <| Errors.extractOnlyAt (Field.Address field) errors
                        ]

            clickableTextFor field =
                Html.li
                    [ Evts.onClick <| editField ( field, qValueOf field ) ]
                    [ Html.text <| CD.fieldToLabel field ++ " : " ++ qValueOf field
                    , Errors.viewMany <| Errors.extractOnlyAt (Field.Address field) errors
                    ]

            inputOrTextFor : CD.Field -> String -> Html msg
            inputOrTextFor field inputId =
                case currentlyEditing of
                    Just ( currentField, currentValue ) ->
                        if currentField == field then
                            inputFor field inputId currentValue
                        else
                            clickableTextFor field
                    Nothing ->
                        clickableTextFor field

        in
        Html.ul
            []
            [ Html.li []
                [ Html.label  [ Attrs.for "preferred-contact" ] [ Html.text "Preferred way of contact:" ]
                , Html.select
                    [ Attrs.id "preferred-contact"
                    , Evts.onInput (setContactMethod << CD.preferredContactFromOption)
                     ] <| List.map preferredContactOption CD.preferredWaysToContact
                ]
            , inputOrTextFor CD.F_Email "contact-email"
            , inputOrTextFor CD.F_Phone "contact-phone"
            , inputOrTextFor CD.F_CompanyName "contact-company"
            , inputOrTextFor CD.F_StreetAddress "contact-street-address"
            , inputOrTextFor CD.F_ZipCode "contact-zip-code"
            , inputOrTextFor CD.F_City "contact-city"
            , inputOrTextFor CD.F_Country "contact-country"
            ]