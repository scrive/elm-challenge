module Main exposing (main)

import Browser
import Data
import Set
import Either exposing (Either(..))

import Html exposing (Html)
import Html.Attributes as Attrs
import Html.Events as Evts

import Scrive.UserGroup exposing (UserGroup)
import Scrive.UserGroup as UG
import Scrive.ContactDetails as CD
import Scrive.Address as CD
import Scrive.RetentionPolicy as RP

import Json.Decode as Json
import Json.Encode as Json

import Scrive.Tag exposing (Tag, ArchivedTag, SomeTag)
import Scrive.Tag as Tag

import Scrive.Form.Field as BelongsTo exposing (BelongsTo(..))
import Scrive.Form.Error as Form exposing (Error)
import Scrive.Form.Error as FE
import Scrive.Form.Tags as TagsForm
import Scrive.Form.ContactDetails as ContactForm
import Scrive.Form.RetentionPolicy as RetentionPolicy
import Scrive.Form.Validator exposing (tagValidator, addressValidator)

import Validate exposing (Validator, Valid)
import Validate as V
import Validate.Extra as V

---- MODEL ----


type alias Model =
    { userGroup : Result Json.Error UserGroup
    , tagInProgress : TagsForm.TagInProgress
    , policyToAdd : Maybe RP.DataRetentionPolicy
    , policyInProgress : Maybe ( RP.DataRetentionPolicy, Int )
    , errors : List Form.Error
    , viewMode : ViewJson -- TODO: remove, only for debugging
    }


init : ( Model, Cmd Msg )
init =
    (
        { userGroup = Json.decodeString UG.decoder Data.userGroup
        , tagInProgress = TagsForm.none
        , policyToAdd = Nothing
        , policyInProgress = Nothing
        , errors = []
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
    | TagInProgress TagsForm.TagInProgress
    | TryToCreateTag { newName : String, newValue : String }
    | TryToRestoreTag ArchivedTag { newValue : String }
    | TryToChangeTag Tag { newValue : String }
    | TryToArchiveTag Tag
    | RemoveTag SomeTag
    | TryToUpdateAddress CD.Address
    | SelectPolicyToAdd RP.DataRetentionPolicy
    | ClearPolicyToAdd
    | AddSelectedPolicy
    | StartDefiningPolicy RP.DataRetentionPolicy
    | TryToDefinePolicyTimeout RP.DataRetentionPolicy Int
    | ApplyPolicyTimeout RP.DataRetentionPolicy
    | ClearPolicyTimeout RP.DataRetentionPolicy
    | ToggleImmediateTrash Bool
    | ToggleJsonMode ViewJson



update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    ( case msg of

        TagInProgress inProgress ->
            { model | tagInProgress = inProgress }

        TryToCreateTag { newName, newValue } ->
            validateTagAnd addTag (tagValidator { isNew = True, nameId = newName }) model
                <| Tag.make newName newValue

        TryToChangeTag theTag { newValue } ->
            validateTagAnd changeTag (tagValidator { isNew = False, nameId = Tag.nameOf theTag }) model
                <| Tag.setValue newValue theTag

        TryToRestoreTag ttr { newValue } ->
            validateTagAnd restoreTag (tagValidator { isNew = False, nameId = Tag.nameOfArchived ttr }) model
                <| Tag.make (Tag.nameOfArchived ttr) newValue

        TryToArchiveTag theTag ->

            { model
            | tagInProgress = TagsForm.none
            , userGroup =
                Result.map
                    (\ug -> { ug | tags = archiveTag theTag ug.tags })
                    model.userGroup
            }

        TryToUpdateAddress newAddress ->
            let
                resValidAddress = V.validate addressValidator newAddress
            in
                case resValidAddress of
                    Ok validAddress ->
                        { model
                        | errors = []
                        , userGroup = Result.map (updateAddress validAddress) model.userGroup
                        }
                    Err errors ->
                        { model | errors = errors }

        RemoveTag theTag ->

            { model
            | tagInProgress = TagsForm.none
            , userGroup =
                Result.map
                    (\ug -> { ug | tags = removeTag theTag ug.tags })
                    model.userGroup
            }

        ToggleJsonMode jsonMode ->
            { model | viewMode = jsonMode }

        SelectPolicyToAdd policy ->
            { model
            | policyToAdd = Just policy
            }

        AddSelectedPolicy ->
            { model
            | policyToAdd = Nothing
            , userGroup =
                case model.policyToAdd of
                    Just policyToAdd -> Result.map (changePolicies <| RP.setPolicyTimeout policyToAdd 0) model.userGroup
                    Nothing -> model.userGroup
            }

        ClearPolicyToAdd ->
            { model
            | policyToAdd = Nothing
            }

        StartDefiningPolicy policy ->
            { model
            | policyInProgress = Just ( policy, 0 )
            }

        TryToDefinePolicyTimeout policy timeout ->
            { model
            | policyInProgress = Just ( policy, timeout )
            , userGroup = Result.map (changePolicies <| RP.setPolicyTimeout policy timeout) model.userGroup
            }

        ApplyPolicyTimeout expectedPolicy ->
            { model
            | policyInProgress = Nothing
            , userGroup =
                case model.policyInProgress of
                    Just ( policy, timeout ) ->
                        if policy == expectedPolicy then
                            Result.map (changePolicies <| RP.setPolicyTimeout policy timeout) model.userGroup
                        else model.userGroup
                    Nothing -> model.userGroup
            }

        ClearPolicyTimeout policy ->
            { model
            | policyInProgress = Nothing
            , userGroup = Result.map (changePolicies <| RP.clearPolicyTimeout policy) model.userGroup
            }

        ToggleImmediateTrash doTrash ->
            { model
            | policyInProgress = Nothing
            , userGroup = Result.map (changePolicies <| if doTrash then RP.setImmediateTrash else RP.clearImmediateTrash) model.userGroup
            }

        NoOp -> model

    , Cmd.none
    )


validateTagAnd : (Valid Tag -> List SomeTag -> List SomeTag) -> (List SomeTag -> Validator Form.Error Tag) -> Model -> Tag -> Model
validateTagAnd fValid fValidator model theTag =
    case Result.map .tags model.userGroup of
        Ok currentTags ->
            let
                resValidTag = V.validate (fValidator currentTags) theTag
            in
                case resValidTag of
                    Ok validTag ->
                        { model
                        | tagInProgress = TagsForm.none -- We clear the form state on any validation success. FIXME: looks like not an obvious place to do it
                        , errors = []
                        , userGroup =
                            Result.map
                                (\ug -> { ug | tags = fValid validTag ug.tags })
                                model.userGroup
                        }
                    Err errors ->
                        { model
                        | errors = errors -- And store errors on any validation failure. FIXME: looks like not an obvious place to do it
                        }
        Err _ ->
            model


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


updateAddress : Valid CD.Address -> UG.UserGroup -> UG.UserGroup
updateAddress nextAddress ugroup =
    let
        curDetails = ugroup.contactDetails
    in
        { ugroup | contactDetails = { curDetails | address = V.fromValid nextAddress } }



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
            , tryArchive = TryToArchiveTag
            , remove = RemoveTag
            , markInProgress = TagInProgress
            }
        policyHandlers =
            { startDefiningPolicy = StartDefiningPolicy
            , tryDefineTimeout = TryToDefinePolicyTimeout
            , applyCurrentTimeout = ApplyPolicyTimeout
            , clearTimeout = ClearPolicyTimeout
            , selectPolicyToAdd = SelectPolicyToAdd
            , addSelectedPolicy = AddSelectedPolicy
            , clearPolicyToAdd = ClearPolicyToAdd
            , toggleImmediateTrash = ToggleImmediateTrash
            }
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
                        (model.errors |> FE.onlyBelongingTo BelongsTo.Contacts)
                        { readOnly = False, toMsg = TryToUpdateAddress }
                        userGroup.contactDetails

                    , Html.h1 [] [ Html.text "Tags" ]
                    , TagsForm.view
                        (model.errors |> FE.onlyBelongingTo BelongsTo.Tags)
                        tagHandlers
                        model.tagInProgress
                        userGroup.tags

                    , Html.h1 [] [ Html.text "Settings" ]
                    , RetentionPolicy.view
                        (model.errors |> FE.onlyBelongingTo BelongsTo.Settings)
                        policyHandlers
                        { adding = model.policyToAdd
                        , editing = Maybe.map Tuple.first model.policyInProgress
                        }
                        <| RP.toList userGroup.settings.policy

                    ]
                Err error ->
                    [ Html.text <| Json.errorToString error ]
            ) ++
            [ Html.div
                [ Attrs.class "absolute top-0 left-0" ]
                [ Html.button [ Evts.onClick <| ToggleJsonMode NoJson ] [ Html.text "No JSON" ]
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
