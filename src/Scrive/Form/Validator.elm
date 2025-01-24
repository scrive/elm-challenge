module Scrive.Form.Validator exposing (..)

import Set
import Either exposing (Either(..))

import Scrive.Data.UserGroup as UG
import Scrive.Data.ContactDetails as CD
import Scrive.Data.Address as CD

import Scrive.Data.Tag exposing (Tag, SomeTag)
import Scrive.Data.Tag as Tag

import Scrive.Form.Field as Field
import Scrive.Form.Error as Form exposing (Error)
import Scrive.Form.Error as FE

import Validate exposing (Validator)
import Validate as V
import Validate.Extra as V


tagValidator : { isNew : Bool, index : Maybe Int } -> List SomeTag -> Validator Form.Error Tag
tagValidator { isNew, index } currentTags =
    let
        nameField  = if isNew then Field.NewTagName  else Field.NameOfTag <| Maybe.withDefault -1 index
        valueField = if isNew then Field.NewTagValue else Field.ValueOfTag <| Maybe.withDefault -1 index
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
        , V.ifNotAllCharsAre (\c -> Char.isAlphaNum c || c == ' ') Tag.nameOf <| FE.make nameField "Tag name should consist of only letters, numbers or spaces"
        , V.ifBlank Tag.valueOf <| FE.make valueField "Value of the tag should not be empty"
        ]


addressValidator : CD.PreferredContact -> Validator Form.Error CD.DraftAddress
addressValidator preferredMethod =
    let
        notSpecified : Maybe String -> Bool
        notSpecified mbVal =
            case mbVal of
                Just str -> not <| String.length str > 0
                Nothing -> True
    in Validate.all
        [ V.ifTrue
            (\addr ->
                preferredMethod == CD.PC_Email && notSpecified addr.email
            )
            <| FE.make (Field.Address CD.F_Email) "E-mail should be specified when preferred contact method is set to \"e-mail\""
        , V.ifTrue
            (\addr ->
                preferredMethod == CD.PC_Post && notSpecified addr.address
            )
            <| FE.make (Field.Address CD.F_StreetAddress) "Street address should be specified when preferred contact method is set to \"post\""
        , V.ifTrue
            (\addr ->
                preferredMethod == CD.PC_Phone && notSpecified addr.phone
            )
            <| FE.make (Field.Address CD.F_Phone) "Phone should be specified when preferred contact method is set to \"phone\""
        , V.ifFalse (.email >> Maybe.map (\e -> V.isValidEmail e || String.isEmpty e) >> Maybe.withDefault True)
            <| FE.make (Field.Address CD.F_Email) "Specified e-mail has invalid format"
        , V.ifFalse (.phone >> Maybe.map (\p -> V.isValidPhone p || String.isEmpty p) >> Maybe.withDefault True)
            <| FE.make (Field.Address CD.F_Phone) "Specified phone has invalid format"
        , V.ifNotAllCharsAre (\c -> Char.isDigit c || c == ' ') (.zip >> Maybe.withDefault "")
            <| FE.make (Field.Address CD.F_ZipCode) "ZIP code should contain only digits or spaces"
        ]
