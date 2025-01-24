module Main exposing (main)

import Browser
import Data
import Set
import Either exposing (Either(..))

import Html exposing (Html)
import Html.Attributes as Attrs
import Html.Events as Evts

import Scrive.Data.UserGroup exposing (UserGroup)
import Scrive.Data.UserGroup as UG
import Scrive.Data.ContactDetails as CD
import Scrive.Data.Address as CD
import Scrive.Data.RetentionPolicy as RP

import Json.Decode as Json
import Json.Encode as Json

import Scrive.Data.Tag exposing (Tag, ArchivedTag, SomeTag)
import Scrive.Data.Tag as Tag

import Scrive.Form.Field as Form
import Scrive.Form.Field as Field
import Scrive.Form.Field as BelongsTo exposing (BelongsTo(..))
import Scrive.Form.Error as Form
import Scrive.Form.Error as FE
import Scrive.Form.Impl.Tags as TagsForm
import Scrive.Form.Impl.ContactDetails as ContactForm
import Scrive.Form.Impl.RetentionPolicy as RetentionPolicy
import Scrive.Form.Validator exposing (tagValidator, addressValidator)

import Validate exposing (Validator, Valid)
import Validate as V
import Validate.Extra as V
import Scrive.Form.Impl.Tags as Tags

---- MODEL ----


type EditFocus
    = FocusTag TagsForm.TagInProgress
    | FocusPolicyAdd RP.DataRetentionPolicy
    | FocusPolicyEdit ( RP.DataRetentionPolicy, Int )
    | FocusContactsEdit ( CD.Field, String )
    | NotEditing


type alias Model =
    { userGroup : Result Json.Error UserGroup
    , editFocus : EditFocus
    , validationErrors : List Form.Error
    , viewMode : ViewJson -- TODO: remove, only for debugging
    }


init : ( Model, Cmd Msg )
init =
    (
        { userGroup = Json.decodeString UG.decoder Data.userGroup
        , editFocus = NotEditing
        , validationErrors = []
        , viewMode = CurrentJson
        }
    , Cmd.none
    )


type ViewJson
    = NoJson
    | OriginalJson
    | CurrentJson


---- UPDATE ----


type Msg
    = NoOp
    | ClearEditFocus
    | SetContactMethod CD.PreferredContact
    | EditContactsField ( CD.Field, String )
    | SetTagInProgress TagsForm.TagInProgress
    | TryToCreateTag { newName : String, newValue : String }
    | TryToRestoreTag ArchivedTag { newValue : String }
    | TryToChangeTag Tag { newValue : String }
    | ArchiveTag Tag
    | RemoveTag SomeTag
    | TryToUpdateAddress CD.DraftAddress
    | SelectPolicyToAdd RP.DataRetentionPolicy
    | AddSelectedPolicy
    | EditPolicyTimeout RP.DataRetentionPolicy Int
    | ApplyPolicyTimeout RP.DataRetentionPolicy
    | ClearPolicyTimeout RP.DataRetentionPolicy
    | ToggleImmediateTrash Bool
    | ToggleJsonMode ViewJson



update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    ( case msg of

        ClearEditFocus ->
            { model | editFocus = NotEditing }

        EditContactsField ( field, value ) ->
            { model | editFocus = FocusContactsEdit ( field, value ) }

        SetContactMethod pmethod ->
            { model
            | userGroup = Result.map (updateContactMethod pmethod) model.userGroup
            , validationErrors =
                case model.userGroup of
                    Ok userGroup ->
                        let
                            resValidDraftAddress = -- just revalidating address with new contact method
                                CD.toDraft userGroup.contactDetails.address
                                    |> V.validate (addressValidator pmethod)
                        in
                            case resValidDraftAddress of
                                Ok _ -> []
                                Err errors -> errors
                    Err _ -> []
            }

        TryToUpdateAddress newDraftAddress ->
            let
                preferredMethod =
                    Result.map (.contactDetails >> .address >> .preferredContactMethod) model.userGroup
                        |> Result.withDefault CD.PC_None -- if result is erronous, we won't update anything anyway
                resValidDraftAddress = V.validate (addressValidator preferredMethod) newDraftAddress
            in
                case resValidDraftAddress of
                    Ok validDraftAddress ->
                        { model
                        | validationErrors = []
                        , editFocus = NotEditing
                        , userGroup = Result.map (updateAddress validDraftAddress) model.userGroup
                        }
                    Err errors ->
                        { model | validationErrors = errors }

        SetTagInProgress inProgress ->
            { model | editFocus = FocusTag inProgress }

        TryToCreateTag { newName, newValue } ->
            let
                resNextUserGroup =
                    validateTagAnd addTag (tagValidator { isNew = True, index = Nothing }) model
                        <| Tag.make newName newValue
            in
                case resNextUserGroup of
                    Ok nextUserGroup ->
                        { model
                        | editFocus = NotEditing
                        , validationErrors = []
                        , userGroup = Ok nextUserGroup
                        }
                    Err validationErrors ->
                        { model | validationErrors = validationErrors }

        TryToChangeTag theTag { newValue } ->
            let
                resNextUserGroup =
                    validateTagAnd changeTag (tagValidator { isNew = False, index = indexOfTagInProgress model }) model
                        <| Tag.setValue newValue theTag
            in
                case resNextUserGroup of
                    Ok nextUserGroup ->
                        { model
                        | editFocus = NotEditing
                        , validationErrors = []
                        , userGroup = Ok nextUserGroup
                        }
                    Err validationErrors ->
                        { model | validationErrors = validationErrors }

        TryToRestoreTag ttr { newValue } ->
          let
                resNextUserGroup =
                    validateTagAnd restoreTag (tagValidator { isNew = False, index = indexOfTagInProgress model }) model
                        <| Tag.make (Tag.nameOfArchived ttr) newValue
            in
                case resNextUserGroup of
                    Ok nextUserGroup ->
                        { model
                        | editFocus = NotEditing
                        , validationErrors = []
                        , userGroup = Ok nextUserGroup
                        }
                    Err validationErrors ->
                        { model | validationErrors = validationErrors }

        ArchiveTag theTag ->

            { model
            | editFocus = NotEditing
            , userGroup =
                Result.map
                    (\ug -> { ug | tags = archiveTag theTag ug.tags })
                    model.userGroup
            }

        RemoveTag theTag ->

            { model
            | editFocus = NotEditing
            , userGroup =
                Result.map
                    (\ug -> { ug | tags = removeTag theTag ug.tags })
                    model.userGroup
            }

        ToggleJsonMode jsonMode ->
            { model | viewMode = jsonMode }

        SelectPolicyToAdd policy ->
            { model
            | editFocus = FocusPolicyAdd policy
            }

        AddSelectedPolicy ->
            { model
            | editFocus = NotEditing
            , userGroup =
                case model.editFocus of
                    FocusPolicyAdd policyToAdd -> Result.map (changePolicies <| RP.setPolicyTimeout policyToAdd 0) model.userGroup
                    _ -> model.userGroup
            }

        EditPolicyTimeout policy timeout ->
            { model
            | editFocus = FocusPolicyEdit ( policy, timeout )
            }

        ApplyPolicyTimeout expectedPolicy ->
            { model
            | editFocus = NotEditing
            , userGroup =
                case model.editFocus of
                    FocusPolicyEdit ( policy, timeout ) ->
                        if policy == expectedPolicy then
                            Result.map (changePolicies <| RP.setPolicyTimeout policy timeout) model.userGroup
                        else model.userGroup
                    _ -> model.userGroup
            }

        ClearPolicyTimeout policy ->
            { model
            | editFocus = NotEditing
            , userGroup = Result.map (changePolicies <| RP.clearPolicyTimeout policy) model.userGroup
            }

        ToggleImmediateTrash doTrash ->
            { model
            | editFocus = NotEditing
            , userGroup = Result.map (changePolicies <| if doTrash then RP.setImmediateTrash else RP.clearImmediateTrash) model.userGroup
            }

        NoOp -> model

    , Cmd.none
    )



indexOfTagInProgress : Model -> Maybe Int
indexOfTagInProgress model =
    case model.editFocus of
        FocusTag tip -> TagsForm.indexOfTagInProgress tip
        _ -> Nothing


validateTagAnd
    :  (Valid Tag -> List SomeTag -> List SomeTag)
    -> (List SomeTag -> Validator Form.Error Tag)
    -> Model
    -> Tag
    -> Result (List Form.Error) UserGroup
validateTagAnd fValid fValidator model theTag =
    case Result.map .tags model.userGroup of
        Ok currentTags ->
            let
                resValidTag = V.validate (fValidator currentTags) theTag
            in
                case resValidTag of
                    Ok validTag ->
                        Result.map
                            (\ug -> { ug | tags = fValid validTag ug.tags })
                            model.userGroup
                        |> Result.mapError (always [])
                    Err errors -> Err errors
        Err _ ->
            Err []


addTag : Valid Tag -> List SomeTag -> List SomeTag
addTag validTag list =
    -- V.fromValid >> Right >> (::) -- adds to the start of list instead of the end
    list ++ [ Right <| V.fromValid validTag ]


changeTag : Valid Tag -> List SomeTag -> List SomeTag
changeTag validTag =
    let theTag = V.fromValid validTag
    in List.map <| Either.mapRight
        (\otherTag ->
            if Tag.nameOf otherTag == Tag.nameOf theTag then theTag else otherTag
        )


archiveTag : Tag -> List SomeTag -> List SomeTag
archiveTag theTag =
    List.map
        <| Either.andThenRight
            (\otherTag ->
                if Tag.nameOf otherTag == Tag.nameOf theTag then Left <| Tag.toArchived theTag else Right otherTag
            )


removeTag : SomeTag -> List SomeTag -> List SomeTag
removeTag eTag =
    List.filter
        (\eOtherTag ->
            case ( eTag, eOtherTag ) of
                ( Right theTag, Right otherTag ) -> Tag.nameOf otherTag /= Tag.nameOf theTag
                ( Left theArchivedTag, Left otherArchivedTag )  -> Tag.nameOfArchived theArchivedTag /= Tag.nameOfArchived otherArchivedTag
                _ -> True
        )


restoreTag : {- TagToRemove -> -} Valid Tag -> List SomeTag -> List SomeTag
restoreTag validTag =
    let theTag = V.fromValid validTag
    in List.map
        <| Either.andThenLeft
            (\archivedTag ->
                if Tag.nameOfArchived archivedTag == Tag.nameOf theTag then Right theTag else Left archivedTag
            )


updateAddress : Valid CD.DraftAddress -> UG.UserGroup -> UG.UserGroup
updateAddress validDraftAddress ugroup =
    let
        curDetails = ugroup.contactDetails
    in
        { ugroup | contactDetails =
            { curDetails
                | address = CD.fromDraft curDetails.address.preferredContactMethod validDraftAddress
            }
        }


updateContactMethod : CD.PreferredContact -> UG.UserGroup -> UG.UserGroup
updateContactMethod pcontact ugroup =
    let
        curDetails = ugroup.contactDetails
        curAddress = curDetails.address
    in
        { ugroup | contactDetails =
            { curDetails
                | address =
                    { curAddress | preferredContactMethod = pcontact }
            }
        }



changePolicies : (RP.PolicyRec -> RP.PolicyRec) -> UG.UserGroup -> UG.UserGroup
changePolicies fPolicyRec ugroup =
    let
        curSettings = ugroup.settings
    in
        { ugroup | settings = { curSettings | policy = fPolicyRec curSettings.policy } }



---- VIEW ----

{-
header : String -> Html msg
header text =
    Html.span [ Attrs.class "p-2 text-5xl font-extrabold text-transparent bg-clip-text bg-gradient-to-br from-slate-400 to-slate-800" ]
        [ Html.text text ]


subheader : String -> Html msg
subheader text =
    Html.span [ Attrs.class "p-2 text-2xl font-extrabold text-slate-800" ]
        [ Html.text text ]
-}


view : Model -> Html Msg
view model =
    let
        tagHandlers =
            { tryCreate  = TryToCreateTag
            , tryChange  = TryToChangeTag
            , tryRestore = TryToRestoreTag
            , archive = ArchiveTag
            , remove = RemoveTag
            , setInProgress = SetTagInProgress
            }

        policyHandlers =
            { editCurrentTimeout = EditPolicyTimeout
            , applyCurrentTimeout = ApplyPolicyTimeout
            , clearTimeout = ClearPolicyTimeout
            , selectPolicyToAdd = SelectPolicyToAdd
            , addSelectedPolicy = AddSelectedPolicy
            , clearPolicyToAdd = ClearEditFocus
            , toggleImmediateTrash = ToggleImmediateTrash
            }

        contactsHandlers =
            { setContactMethod = SetContactMethod
            , tryUpdate = TryToUpdateAddress
            , editField = EditContactsField
            }

        mbTagInProgress =
            case model.editFocus of
                FocusTag tip -> Just tip
                _ -> Nothing

        mbAddingPolicy =
            case model.editFocus of
                FocusPolicyAdd policy -> Just policy
                _ -> Nothing

        mbEditingPolicy =
            case model.editFocus of
                FocusPolicyEdit ( policy, pvalue ) -> Just ( policy, pvalue )
                _ -> Nothing

        mbContactsField =
            case model.editFocus of
                FocusContactsEdit ( field, fvalue ) -> Just ( field, fvalue )
                _ -> Nothing

    in

    Html.div
        [ ]
        [ Html.div [ Attrs.class "flex flex-col w-[1024px] items-center mx-auto mt-16 mb-48" ]
            -- [ header "Let's start your task"
            -- , subheader "Here are your data:"
            -- , header "Now turn them into form."
            -- , subheader "See README for details of the task. Good luck 🍀 "
            <|
            ( case model.userGroup of
                Ok userGroup ->

                    [ Html.h1 [] [ Html.text "Contacts" ]
                    , ContactForm.view
                        (model.validationErrors |> FE.onlyBelongingTo BelongsTo.Contacts)
                        contactsHandlers
                        { readOnly = False, currentlyEditing = mbContactsField }
                        userGroup.contactDetails

                    , Html.h1 [] [ Html.text "Tags" ]
                    , TagsForm.view
                        (model.validationErrors |> FE.onlyBelongingTo BelongsTo.Tags)
                        tagHandlers
                        mbTagInProgress
                        userGroup.tags

                    , Html.h1 [] [ Html.text "Retention Policy" ]
                    , RetentionPolicy.view
                        (model.validationErrors |> FE.onlyBelongingTo BelongsTo.Settings)
                        policyHandlers
                        { currentlyAdding = mbAddingPolicy
                        , currentlyEditing = mbEditingPolicy
                        }
                        <| RP.toList userGroup.settings.policy

                    ]
                Err error ->
                    [ Html.text <| Json.errorToString error ]
            ) ++
            [ Html.div -- temporary div with current JSON
                [ Attrs.class "absolute top-0 left-0" ]
                [ Html.text
                    <| case model.editFocus of
                        FocusTag tip -> "Tag : " ++ Tags.tagInProgressToString tip
                        FocusPolicyAdd policy -> "Policy add : " ++ RP.toString policy
                        FocusPolicyEdit ( policy, intVal ) -> "Policy edit : " ++ RP.toString policy ++ " (" ++ String.fromInt intVal ++ ")"
                        FocusContactsEdit ( cdField, value ) -> "Contacts edit : " ++ CD.fieldToLabel cdField ++ " (" ++ value ++ ")"
                        NotEditing -> "Not editing"
                , Html.hr [] []
                , Html.button [ Evts.onClick <| ToggleJsonMode NoJson ] [ Html.text "No JSON" ]
                , Html.button [ Evts.onClick <| ToggleJsonMode OriginalJson ] [ Html.text "Original JSON" ]
                , Html.button [ Evts.onClick <| ToggleJsonMode CurrentJson ] [ Html.text "Current JSON" ]
                , Html.pre
                    [ Attrs.class "my-8 py-4 px-9 text-xs bg-slate-100 font-mono shadow rounded" ]
                    [ Html.text <|
                        case model.viewMode of
                            NoJson -> ""
                            CurrentJson ->
                                case model.userGroup of
                                    Ok userGroup -> Json.encode 2 <| UG.encode userGroup
                                    Err _ -> "Initial: " ++ Data.userGroup
                            OriginalJson ->
                                Data.userGroup
                    ]
                ]
            ]
        ]



---- PROGRAM ----


main : Program () Model Msg
main =
    Browser.application
        { view =
            \model ->
                { title = "Scrive elm challenge task"
                , body = [ view model ]
                }
        , init = \_ _ _ -> init
        , update = update
        , subscriptions = always Sub.none
        , onUrlRequest = always NoOp
        , onUrlChange = always NoOp
        }
