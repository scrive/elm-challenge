module Main exposing (main)

import Browser
import Data
import Either exposing (Either(..))
import Html exposing (Html)
import Html.Attributes as Attrs
import Html.Events as Evts
import Json.Decode as Json
import Json.Encode as Json
import Scrive.Data.Address as A
import Scrive.Data.RetentionPolicy as RP
import Scrive.Data.Tag as Tag exposing (ArchivedTag, SomeTag, Tag)
import Scrive.Data.UserGroup as UG exposing (UserGroup)
import Scrive.Form.Error as FE
import Scrive.Form.Field as BelongsTo exposing (BelongsTo(..))
import Scrive.Form.Impl.ContactDetails as ContactForm
import Scrive.Form.Impl.RetentionPolicy as RetentionPolicy
import Scrive.Form.Impl.Tags as TagsForm
import Scrive.Form.Validator exposing (addressValidator, tagValidator)
import Set
import Style as Style
import Validate as V exposing (Valid, Validator)
import Validate.Extra as V



---- MODEL ----


type EditFocus
    = FocusTag TagsForm.TagInProgress
    | FocusPolicyAdd RP.DataRetentionPolicy
    | FocusPolicyEdit ( RP.DataRetentionPolicy, Int )
    | FocusContactsEdit ( A.Field, String )
    | NotEditing


type alias FormsModel =
    { userGroup : UserGroup
    , editFocus : EditFocus
    , validationErrors : List FE.Error
    , debugMode : ViewJson
    }


type alias Model =
    Result Json.Error FormsModel


init : ( Model, Cmd Msg )
init =
    ( Json.decodeString UG.decoder Data.userGroup
        |> Result.map
            (\userGroup ->
                { userGroup = userGroup
                , editFocus = NotEditing
                , validationErrors = []
                , debugMode = CurrentJson
                }
            )
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
    | SetContactMethod A.PreferredContact
    | EditContactsField ( A.Field, String )
    | SetTagInProgress TagsForm.TagInProgress
    | TryToCreateTag { newName : String, newValue : String }
    | TryToRestoreTag ArchivedTag { newValue : String }
    | TryToChangeTag Tag { newValue : String }
    | ArchiveTag Tag
    | RemoveTag SomeTag
    | TryToUpdateAddress A.DraftAddress
    | SelectPolicyToAdd RP.DataRetentionPolicy
    | AddSelectedPolicy
    | EditPolicyTimeout RP.DataRetentionPolicy Int
    | ApplyPolicyTimeout RP.DataRetentionPolicy
    | ClearPolicyTimeout RP.DataRetentionPolicy
    | ToggleImmediateTrash Bool
    | ToggleJsonMode ViewJson


updateForms : Msg -> FormsModel -> FormsModel
updateForms msg fmodel =
    case msg of
        ClearEditFocus ->
            { fmodel | editFocus = NotEditing }

        EditContactsField ( field, value ) ->
            { fmodel | editFocus = FocusContactsEdit ( field, value ) }

        SetContactMethod pmethod ->
            { fmodel
                | userGroup = updateContactMethod pmethod fmodel.userGroup
                , validationErrors =
                    let
                        resValidDraftAddress =
                            -- just revalidating address with new contact method
                            A.toDraft fmodel.userGroup.contactDetails.address
                                |> V.validate (addressValidator pmethod)
                    in
                    case resValidDraftAddress of
                        Ok _ ->
                            []

                        Err errors ->
                            errors
            }

        TryToUpdateAddress newDraftAddress ->
            let
                preferredMethod =
                    fmodel.userGroup.contactDetails.address.preferredContactMethod

                resValidDraftAddress =
                    V.validate (addressValidator preferredMethod) newDraftAddress
            in
            case resValidDraftAddress of
                Ok validDraftAddress ->
                    { fmodel
                        | validationErrors = []
                        , editFocus = NotEditing
                        , userGroup = updateAddress validDraftAddress fmodel.userGroup
                    }

                Err errors ->
                    { fmodel | validationErrors = errors }

        SetTagInProgress inProgress ->
            { fmodel | editFocus = FocusTag inProgress }

        TryToCreateTag { newName, newValue } ->
            Tag.make newName newValue
                |> validateTagAnd addTag (tagValidator { isNew = True, index = Nothing }) fmodel
                |> applyResultOrStoreErrors fmodel

        TryToChangeTag theTag { newValue } ->
            Tag.setValue newValue theTag
                |> validateTagAnd changeTag (tagValidator { isNew = False, index = indexOfTagInProgress fmodel }) fmodel
                |> applyResultOrStoreErrors fmodel

        TryToRestoreTag archivedTag { newValue } ->
            Tag.make (Tag.nameOfArchived archivedTag) newValue
                |> validateTagAnd restoreTag (tagValidator { isNew = False, index = indexOfTagInProgress fmodel }) fmodel
                |> applyResultOrStoreErrors fmodel

        ArchiveTag theTag ->
            let
                currentUserGroup =
                    fmodel.userGroup
            in
            { fmodel
                | editFocus = NotEditing
                , userGroup =
                    { currentUserGroup
                        | tags = archiveTag theTag currentUserGroup.tags
                    }
            }

        RemoveTag theTag ->
            let
                currentUserGroup =
                    fmodel.userGroup
            in
            { fmodel
                | editFocus = NotEditing
                , userGroup =
                    { currentUserGroup
                        | tags = removeTag theTag currentUserGroup.tags
                    }
            }

        SelectPolicyToAdd policy ->
            { fmodel
                | editFocus = FocusPolicyAdd policy
            }

        AddSelectedPolicy ->
            { fmodel
                | editFocus = NotEditing
                , userGroup =
                    case fmodel.editFocus of
                        FocusPolicyAdd policyToAdd ->
                            changePolicies (RP.setPolicyTimeout policyToAdd 0) fmodel.userGroup

                        _ ->
                            fmodel.userGroup
            }

        EditPolicyTimeout policy timeout ->
            { fmodel
                | editFocus = FocusPolicyEdit ( policy, timeout )
            }

        ApplyPolicyTimeout expectedPolicy ->
            { fmodel
                | editFocus = NotEditing
                , userGroup =
                    case fmodel.editFocus of
                        FocusPolicyEdit ( policy, timeout ) ->
                            if policy == expectedPolicy then
                                changePolicies (RP.setPolicyTimeout policy timeout) fmodel.userGroup

                            else
                                fmodel.userGroup

                        _ ->
                            fmodel.userGroup
            }

        ClearPolicyTimeout policy ->
            { fmodel
                | editFocus = NotEditing
                , userGroup = changePolicies (RP.clearPolicyTimeout policy) fmodel.userGroup
            }

        ToggleImmediateTrash doTrash ->
            { fmodel
                | editFocus = NotEditing
                , userGroup =
                    (changePolicies <|
                        if doTrash then
                            RP.setImmediateTrash

                        else
                            RP.clearImmediateTrash
                    )
                        fmodel.userGroup
            }

        ToggleJsonMode jsonMode ->
            { fmodel | debugMode = jsonMode }

        NoOp ->
            fmodel


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    ( model |> Result.map (updateForms msg)
    , Cmd.none
    )


applyResultOrStoreErrors : FormsModel -> Result (List FE.Error) UserGroup -> FormsModel
applyResultOrStoreErrors fmodel resNextUserGroup =
    case resNextUserGroup of
        Ok nextUserGroup ->
            { fmodel
                | editFocus = NotEditing -- FIXME: silently resets edit focus
                , validationErrors = []
                , userGroup = nextUserGroup
            }

        Err validationErrors ->
            { fmodel | validationErrors = validationErrors }


indexOfTagInProgress : FormsModel -> Maybe Int
indexOfTagInProgress model =
    case model.editFocus of
        FocusTag tip ->
            TagsForm.indexOfTagInProgress tip

        _ ->
            Nothing


validateTagAnd :
    (Valid Tag -> List SomeTag -> List SomeTag)
    -> (List SomeTag -> Validator FE.Error Tag)
    -> FormsModel
    -> Tag
    -> Result (List FE.Error) UserGroup
validateTagAnd fValid fValidator fmodel theTag =
    let
        userGroup =
            fmodel.userGroup

        currentTags =
            fmodel.userGroup.tags

        resValidTag =
            V.validate (fValidator currentTags) theTag
    in
    resValidTag
        |> Result.map
            (\validTag ->
                { userGroup
                    | tags = fValid validTag userGroup.tags
                }
            )


addTag : Valid Tag -> List SomeTag -> List SomeTag
addTag validTag list =
    -- V.fromValid >> Right >> (::) -- adds to the start of list instead of the end
    list ++ [ Right <| V.fromValid validTag ]


changeTag : Valid Tag -> List SomeTag -> List SomeTag
changeTag validTag =
    let
        theTag =
            V.fromValid validTag
    in
    List.map <|
        Either.mapRight
            (\otherTag ->
                if Tag.nameOf otherTag == Tag.nameOf theTag then
                    theTag

                else
                    otherTag
            )


archiveTag : Tag -> List SomeTag -> List SomeTag
archiveTag theTag =
    List.map <|
        Either.andThenRight
            (\otherTag ->
                if Tag.nameOf otherTag == Tag.nameOf theTag then
                    Left <| Tag.toArchived theTag

                else
                    Right otherTag
            )


removeTag : SomeTag -> List SomeTag -> List SomeTag
removeTag eTag =
    List.filter
        (\eOtherTag ->
            case ( eTag, eOtherTag ) of
                ( Right theTag, Right otherTag ) ->
                    Tag.nameOf otherTag /= Tag.nameOf theTag

                ( Left theArchivedTag, Left otherArchivedTag ) ->
                    Tag.nameOfArchived theArchivedTag /= Tag.nameOfArchived otherArchivedTag

                _ ->
                    True
        )


restoreTag : {- TagToRemove -> -} Valid Tag -> List SomeTag -> List SomeTag
restoreTag validTag =
    let
        theTag =
            V.fromValid validTag
    in
    List.map <|
        Either.andThenLeft
            (\archivedTag ->
                if Tag.nameOfArchived archivedTag == Tag.nameOf theTag then
                    Right theTag

                else
                    Left archivedTag
            )


updateAddress : Valid A.DraftAddress -> UG.UserGroup -> UG.UserGroup
updateAddress validDraftAddress ugroup =
    let
        curDetails =
            ugroup.contactDetails
    in
    { ugroup
        | contactDetails =
            { curDetails
                | address = A.fromDraft curDetails.address.preferredContactMethod validDraftAddress
            }
    }


updateContactMethod : A.PreferredContact -> UG.UserGroup -> UG.UserGroup
updateContactMethod pcontact ugroup =
    let
        curDetails =
            ugroup.contactDetails

        curAddress =
            curDetails.address
    in
    { ugroup
        | contactDetails =
            { curDetails
                | address =
                    { curAddress | preferredContactMethod = pcontact }
            }
    }


changePolicies : (RP.PolicyRec -> RP.PolicyRec) -> UG.UserGroup -> UG.UserGroup
changePolicies fPolicyRec ugroup =
    let
        curSettings =
            ugroup.settings
    in
    { ugroup | settings = { curSettings | policy = fPolicyRec curSettings.policy } }



---- VIEW ----


viewForms : FormsModel -> Html Msg
viewForms fmodel =
    let
        tagHandlers =
            { tryCreate = TryToCreateTag
            , tryChange = TryToChangeTag
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
            case fmodel.editFocus of
                FocusTag tip ->
                    Just tip

                _ ->
                    Nothing

        mbAddingPolicy =
            case fmodel.editFocus of
                FocusPolicyAdd policy ->
                    Just policy

                _ ->
                    Nothing

        mbEditingPolicy =
            case fmodel.editFocus of
                FocusPolicyEdit ( policy, pvalue ) ->
                    Just ( policy, pvalue )

                _ ->
                    Nothing

        mbContactsField =
            case fmodel.editFocus of
                FocusContactsEdit ( field, fvalue ) ->
                    Just ( field, fvalue )

                _ ->
                    Nothing

        formHeader title =
            Html.h1 [ Attrs.class Style.formHeader ] [ Html.text title ]

        userGroup =
            fmodel.userGroup
    in
    Html.div [ Attrs.class Style.mainContainer ] <|
        [ formHeader "Contacts"
        , ContactForm.view
            (fmodel.validationErrors |> FE.onlyBelongingTo BelongsTo.Contacts)
            contactsHandlers
            { readOnly = False, currentlyEditing = mbContactsField }
            userGroup.contactDetails
        , formHeader "Tags"
        , TagsForm.view
            (fmodel.validationErrors |> FE.onlyBelongingTo BelongsTo.Tags)
            tagHandlers
            mbTagInProgress
            userGroup.tags
        , formHeader "Retention Policy"
        , RetentionPolicy.view
            (fmodel.validationErrors |> FE.onlyBelongingTo BelongsTo.Settings)
            policyHandlers
            { currentlyAdding = mbAddingPolicy
            , currentlyEditing = mbEditingPolicy
            }
          <|
            RP.toList userGroup.settings.policy
        ]


view : Model -> Html Msg
view model =
    case model of
        Ok formsModel ->
            Html.div
                []
                [ viewForms formsModel
                , viewDebugInfo formsModel
                ]

        Err jsonError ->
            viewError jsonError


viewError : Json.Error -> Html msg
viewError error =
    Html.text <| Json.errorToString error


viewDebugInfo : FormsModel -> Html Msg
viewDebugInfo fmodel =
    Html.div
        -- temporary div with current Debug info
        [ Attrs.class "absolute top-0 left-0" ]
        [ Html.text <|
            case fmodel.editFocus of
                FocusTag tip ->
                    "Tag : " ++ TagsForm.tagInProgressToString tip

                FocusPolicyAdd policy ->
                    "Policy add : " ++ RP.toString policy

                FocusPolicyEdit ( policy, intVal ) ->
                    "Policy edit : " ++ RP.toString policy ++ " (" ++ String.fromInt intVal ++ ")"

                FocusContactsEdit ( cdField, value ) ->
                    "Contacts edit : " ++ A.fieldToLabel cdField ++ " (" ++ value ++ ")"

                NotEditing ->
                    "Not editing"
        , Html.hr [] []
        , Html.button [ Evts.onClick <| ToggleJsonMode NoJson, Attrs.class Style.button ] [ Html.text "No JSON" ]
        , Html.button [ Evts.onClick <| ToggleJsonMode OriginalJson, Attrs.class Style.button ] [ Html.text "Original JSON" ]
        , Html.button [ Evts.onClick <| ToggleJsonMode CurrentJson, Attrs.class Style.button ] [ Html.text "Current JSON" ]
        , Html.pre
            [ Attrs.class "my-8 py-4 px-9 text-xs bg-slate-100 font-mono shadow rounded" ]
            [ Html.text <|
                case fmodel.debugMode of
                    NoJson ->
                        ""

                    CurrentJson ->
                        Json.encode 2 <| UG.encode fmodel.userGroup

                    OriginalJson ->
                        Data.userGroup
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
