module Data.Form exposing
    ( Entries
    , Entry
    , Form
    , ReplyValidation(..)
    , answerQuestion
    , getVisibleQuestions
    , hasErrors
    , isFormValid
    , isQuestionDisabled
    , populateAddress
    , validateAddress
    )

import AssocList exposing (Dict)
import Basics.Extra exposing (flip)
import Data.Answer as Answer exposing (Answer(..))
import Data.Question exposing (Condition(..), Question, QuestionTag(..), addressQuestions, validate)
import Data.UserGroup as UserGroup exposing (Inheritable(..), UserGroup, uuidToString)
import List.Extra
import Tuple.Extra
import Validate


type alias Form =
    ( Entries, ReplyValidation )


type alias Entries =
    Dict QuestionTag Entry


type alias Entry =
    ( Answer, ReplyValidation )


type ReplyValidation
    = Valid
    | Invalid (List String)
    | NotValidated


hasErrors : ReplyValidation -> Bool
hasErrors validation =
    case validation of
        Valid ->
            False

        NotValidated ->
            False

        Invalid _ ->
            True


answerQuestion : QuestionTag -> Answer -> Form -> Form
answerQuestion tag answer =
    Tuple.first
        >> AssocList.insert tag ( answer, NotValidated )
        >> Tuple.pair NotValidated
        >> Tuple.Extra.flip


populateAddress : UserGroup -> Form -> Form
populateAddress userGroup =
    let
        address =
            UserGroup.addressOfUserGroup.get userGroup

        ( inheritedFrom, isInherited ) =
            case userGroup.contactDetails of
                Uninherited _ ->
                    ( "", "" )

                Inherited uuid _ ->
                    ( uuidToString uuid, "checked" )
    in
    answerQuestion Email (Text address.email)
        >> answerQuestion Phone (Text address.phone)
        >> answerQuestion PreferredContactMethod (Text <| UserGroup.contactMethodToString address.preferredContactMethod)
        >> answerQuestion CompanyName (Text address.companyName)
        >> answerQuestion Address (Text address.address)
        >> answerQuestion Zip (Text address.zip)
        >> answerQuestion City (Text address.city)
        >> answerQuestion Country (Text address.country)
        >> answerQuestion InheritedFrom (Text inheritedFrom)
        >> answerQuestion IsInherited (Text isInherited)


validateEntries : List Question -> Entries -> Entries
validateEntries questions =
    AssocList.map
        (\tag ( answer, _ ) ->
            questions
                |> List.Extra.find (.tag >> (==) tag)
                |> Maybe.map (flip validate answer >> fromResult)
                |> Maybe.withDefault (Invalid [])
                |> Tuple.pair answer
        )


fromResult : Result (List String) value -> ReplyValidation
fromResult result =
    case result of
        Ok _ ->
            Valid

        Err errors ->
            Invalid errors


validateAddress : Form -> Form
validateAddress ( entries, _ ) =
    ( validateEntries addressQuestions entries
    , Validate.validate
        (Validate.all
            [ Validate.ifTrue hasEmptyPreferredContactMethod emptyPreferredContactMethodError
            , Validate.ifTrue hasEmptyInheritance emptyInheritanceError
            ]
        )
        entries
        |> fromResult
    )


hasEmptyPreferredContactMethod : Entries -> Bool
hasEmptyPreferredContactMethod entries =
    case AssocList.get PreferredContactMethod entries of
        Just ( answer, _ ) ->
            case UserGroup.contactMethodFromString <| Answer.toString answer of
                Just UserGroup.Email ->
                    isNotAnswered Email entries

                Just UserGroup.Phone ->
                    isNotAnswered Phone entries

                Just UserGroup.Post ->
                    isNotAnswered Address entries

                Nothing ->
                    False

        Nothing ->
            False


hasEmptyInheritance : Entries -> Bool
hasEmptyInheritance entries =
    case AssocList.get IsInherited entries of
        Just ( answer, _ ) ->
            if Answer.isEmpty answer then
                False

            else
                AssocList.get InheritedFrom entries
                    |> Maybe.map (Tuple.first >> Answer.isEmpty)
                    |> Maybe.withDefault True

        Nothing ->
            True


isNotAnswered : QuestionTag -> Entries -> Bool
isNotAnswered tag =
    AssocList.get tag
        >> Maybe.map (Tuple.first >> Answer.isEmpty)
        >> Maybe.withDefault False


emptyPreferredContactMethodError : String
emptyPreferredContactMethodError =
    "Please provide a value for your preferred contact method."


emptyInheritanceError : String
emptyInheritanceError =
    "Please provide a value for Inherited From field."


isFormValid : Form -> Bool
isFormValid ( entries, validation ) =
    validation
        == Valid
        && (AssocList.toList entries
                |> List.map (Tuple.second >> Tuple.second)
                |> List.all ((==) Valid)
           )


getVisibleQuestions : Entries -> List Question -> List Question
getVisibleQuestions entries =
    List.filter
        (\{ visibility } ->
            case visibility of
                Unconditional ->
                    True

                Conditional ( tag, condition ) ->
                    AssocList.get tag entries
                        |> Maybe.map (Tuple.first >> condition)
                        |> Maybe.withDefault False
        )


isQuestionDisabled : Entries -> Question -> Bool
isQuestionDisabled entries { interactability } =
    case interactability of
        Unconditional ->
            False

        Conditional ( tag, condition ) ->
            AssocList.get tag entries
                |> Maybe.map (Tuple.first >> condition >> not)
                |> Maybe.withDefault True
