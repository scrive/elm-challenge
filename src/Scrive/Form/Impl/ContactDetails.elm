module Scrive.Form.Impl.ContactDetails exposing (..)


import Html exposing (Html)
import Html
import Html.Extra as Html
import Html.Attributes as Attrs
import Html.Events as Evts
import Html.Events.Extra as Evts

import Maybe

import Style as Style

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
        let
            draftAddress = CD.toDraft address

            textFor field =
                roLabelAndValue (CD.fieldToLabel field ++ " : ")
                    <| Maybe.withDefault "-"
                    <| CD.draftValueOf draftAddress field
        in
        Html.ul
            [ Attrs.class Style.readOnlyValuesList ]
            <| (roLabelAndValue "Preferred way of contact : "
                    <| CD.preferredContactToString address.preferredContactMethod
               )
            :: (List.map textFor <| CD.allFields)
    else
        let

            draftAddress = CD.toDraft address

            qValueOf : CD.Field -> String
            qValueOf = Maybe.withDefault "" << CD.draftValueOf draftAddress

            qSubmitValue : CD.Field -> String -> msg
            qSubmitValue field currentValue = CD.setDraftValue draftAddress field currentValue |> tryUpdate

            preferredContactOption : CD.PreferredContact -> Html msg
            preferredContactOption pc =
                Html.option
                    [ Attrs.class Style.selectOption
                    , Attrs.selected <| pc == address.preferredContactMethod
                    ]
                    [ Html.text <| CD.preferredContactToString pc ]

            inputFor field inputId currentValue =
                Html.li [ Attrs.class Style.itemWithInput ]
                    [ Html.label
                        [ Attrs.for inputId, Attrs.class Style.inputLabel ]
                        [ Html.text <| CD.fieldToLabel field ++ " : " ]
                    , Html.input
                        [ Attrs.id inputId
                        , Attrs.type_ "text"
                        , Attrs.class Style.textInput
                        , Evts.onInput (\str -> editField ( field, str ))
                        , Attrs.placeholder <| qValueOf field
                        , Attrs.value currentValue
                        , Evts.onEnter <| qSubmitValue field currentValue
                        ]
                        [ ]
                    , Html.button
                        [ Evts.onClick <| qSubmitValue field currentValue
                        , Attrs.title <| Style.altText Style.SetContactValue
                        , Attrs.class Style.button
                        ]
                        [ Html.text <| Style.buttonLabel Style.SetContactValue ]
                    , Errors.viewMany <| Errors.extractOnlyAt (Field.Address field) errors
                    ]

            clickableTextFor field =
                Html.li
                    [ Evts.onClick <| editField ( field, qValueOf field )
                    , Attrs.class Style.itemWithEditableValue
                    ]
                    [ Html.span
                        [ Attrs.class Style.fieldLabel ]
                        [ Html.text <| CD.fieldToLabel field ]
                    ,  Html.span
                        [ Attrs.class Style.fieldValue ]
                        [ Html.text <| qValueOf field ]
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
            [ Attrs.class Style.inputsForValuesList ]
            [ Html.li [ Attrs.class Style.itemWithInput ]
                [ Html.label [ Attrs.for "preferred-contact", Attrs.class Style.inputLabel ] [ Html.text "Preferred way of contact:" ]
                , Html.select
                    [ Attrs.id "preferred-contact"
                    , Attrs.class Style.selectBox
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


roLabelAndValue label value =
    Html.li
        [ Attrs.class Style.itemWithValue ]
        [ Html.span
            [ Attrs.class Style.fieldLabel ]
            [ Html.text label  ]
        ,  Html.span
            [ Attrs.class Style.fieldValue ]
            [ Html.text value ]
        ]