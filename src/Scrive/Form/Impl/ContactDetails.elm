module Scrive.Form.Impl.ContactDetails exposing (..)

import Html exposing (Html)
import Html.Attributes as Attrs
import Html.Events as Evts
import Html.Events.Extra as Evts
import Html.Extra as Html
import Maybe
import Scrive.Data.Address as Address
import Scrive.Data.ContactDetails exposing (ContactDetails)
import Scrive.Form.Error as Errors exposing (Error)
import Scrive.Form.Field as Field exposing (Field)
import Style as Style


type alias Handlers msg =
    { setContactMethod : Address.PreferredContact -> msg
    , tryUpdate : Address.DraftAddress -> msg
    , editField : ( Address.Field, String ) -> msg
    }


type alias State =
    { readOnly : Bool, currentlyEditing : Maybe ( Address.Field, String ) }


view : List Error -> Handlers msg -> State -> ContactDetails -> Html msg
view errors { setContactMethod, tryUpdate, editField } { readOnly, currentlyEditing } { address } =
    if readOnly then
        let
            draftAddress =
                Address.toDraft address

            textFor field =
                roLabelAndValue (Address.fieldToLabel field ++ " : ") <|
                    Maybe.withDefault "-" <|
                        Address.draftValueOf draftAddress field
        in
        Html.ul
            [ Attrs.class Style.readOnlyValuesList ]
        <|
            (roLabelAndValue "Preferred way of contact : " <|
                Address.preferredContactToString address.preferredContactMethod
            )
                :: (List.map textFor <| Address.allFields)

    else
        let
            draftAddress =
                Address.toDraft address

            qValueOf : Address.Field -> String
            qValueOf =
                Maybe.withDefault "" << Address.draftValueOf draftAddress

            qSubmitValue : Address.Field -> String -> msg
            qSubmitValue field currentValue =
                Address.setDraftValue draftAddress field currentValue |> tryUpdate

            preferredContactOption : Address.PreferredContact -> Html msg
            preferredContactOption pc =
                Html.option
                    [ Attrs.class Style.selectOption
                    , Attrs.selected <| pc == address.preferredContactMethod
                    ]
                    [ Html.text <| Address.preferredContactToString pc ]

            inputFor field inputId currentValue =
                Html.li [ Attrs.class Style.itemWithInput ]
                    [ Html.label
                        [ Attrs.for inputId, Attrs.class Style.inputLabel ]
                        [ Html.text <| Address.fieldToLabel field ++ " : " ]
                    , Html.input
                        [ Attrs.id inputId
                        , Attrs.type_ "text"
                        , Attrs.class Style.textInput
                        , Evts.onInput (\str -> editField ( field, str ))
                        , Attrs.placeholder <| qValueOf field
                        , Attrs.value currentValue
                        , Evts.onEnter <| qSubmitValue field currentValue
                        ]
                        []
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
                        [ Html.text <| Address.fieldToLabel field ]
                    , Html.span
                        [ Attrs.class Style.fieldValue ]
                        [ Html.text <| qValueOf field ]
                    , Errors.viewMany <| Errors.extractOnlyAt (Field.Address field) errors
                    ]

            inputOrTextFor : Address.Field -> String -> Html msg
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
                    , Evts.onInput (setContactMethod << Address.preferredContactFromOption)
                    ]
                  <|
                    List.map preferredContactOption Address.preferredWaysToContact
                ]
            , inputOrTextFor Address.F_Email "contact-email"
            , inputOrTextFor Address.F_Phone "contact-phone"
            , inputOrTextFor Address.F_CompanyName "contact-company"
            , inputOrTextFor Address.F_StreetAddress "contact-street-address"
            , inputOrTextFor Address.F_ZipCode "contact-zip-code"
            , inputOrTextFor Address.F_City "contact-city"
            , inputOrTextFor Address.F_Country "contact-country"
            ]


roLabelAndValue label value =
    Html.li
        [ Attrs.class Style.itemWithValue ]
        [ Html.span
            [ Attrs.class Style.fieldLabel ]
            [ Html.text label ]
        , Html.span
            [ Attrs.class Style.fieldValue ]
            [ Html.text value ]
        ]
