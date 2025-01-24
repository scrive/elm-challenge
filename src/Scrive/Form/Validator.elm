module Scrive.Form.Validator exposing (..)

import Set
import Either exposing (Either(..))

import Scrive.UserGroup as UG
import Scrive.ContactDetails as CD
import Scrive.Address as CD

import Scrive.Tag exposing (Tag, SomeTag)
import Scrive.Tag as Tag

import Scrive.Form.Field as Field
import Scrive.Form.Error as Form exposing (Error)
import Scrive.Form.Error as FE

import Validate exposing (Validator)
import Validate as V
import Validate.Extra as V


tagValidator : { isNew : Bool, nameId : String } -> List SomeTag -> Validator Form.Error Tag
tagValidator { isNew, nameId } currentTags =
    let
        nameField  = if isNew then Field.NewTagName  else Field.NameOfTagWith  { name = nameId }
        valueField = if isNew then Field.NewTagValue else Field.ValueOfTagWith { name = nameId }
    in Validate.all
        [ V.ifLongerThan Tag.nameOf 32 <| FE.make nameField "Tag name should not exceed 32 characters"
        ,
            if isNew then
                let
                    tagNames = Set.fromList <| List.map (Either.unpack Tag.nameOfArchived Tag.nameOf) currentTags
                in
                    V.ifNotUnique Tag.nameOf tagNames <| FE.make nameField "One of tags already has this name, please try another one"
            else V.skip

        , V.ifBlank Tag.nameOf <| FE.make nameField "Tag name should not be empty"
        , V.ifNotAllAlphaNum Tag.nameOf <| FE.make nameField "Tag name should consist of only letters or numbers"
        , V.ifBlank Tag.valueOf <| FE.make valueField "Value of the tag should not be empty"
        ]


addressValidator : Validator Form.Error CD.Address
addressValidator =
    let
        notSpecified : Maybe String -> Bool
        notSpecified mbVal =
            case mbVal of
                Just str -> not <| String.length str > 0
                Nothing -> True
    in Validate.all
        [ V.ifTrue
            (\addr ->
                addr.preferredContactMethod == CD.PC_Email && notSpecified (Maybe.map CD.emailToString <| addr.email)
            )
            <| FE.make Field.AddressEmail "E-mail should be specified when preferred contact method is set to \"e-mail\""
        , V.ifTrue
            (\addr ->
                addr.preferredContactMethod == CD.PC_Post && notSpecified addr.address
            )
            <| FE.make Field.AddressStreet "Street address should be specified when preferred contact method is set to \"post\""
        , V.ifTrue
            (\addr ->
                addr.preferredContactMethod == CD.PC_Phone && notSpecified (Maybe.map CD.phoneToString <| addr.phone)
            )
            <| FE.make Field.AddressPhone "Phone should be specified when preferred contact method is set to \"phone\""
        , V.ifFalse (.email >> Maybe.map CD.emailToString >> Maybe.map V.isValidEmail >> Maybe.withDefault True)
            <| FE.make Field.AddressEmail "Specified e-mail is in invalid format"
        , V.ifFalse (.phone >> Maybe.map CD.phoneToString >> Maybe.map V.isValidPhone >> Maybe.withDefault True)
            <| FE.make Field.AddressPhone "Specified phone is in invalid format"
        , V.ifNotAllDigits (.zip >> Maybe.map CD.zipCodeToString >> Maybe.withDefault "")
            <| FE.make Field.AddressPhone "Specified ZIP code should contain only digits"
        ]
