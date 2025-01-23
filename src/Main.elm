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
import Scrive.Settings as RP

import Json.Decode as Json
import Json.Encode as Json

import Scrive.Tag exposing (Tag, TagToRemove)
import Scrive.Tag as Tag

import Form.Error as Form
import Form.Error as FE exposing (textOf, Field(..))
import Form.Tags as TagsForm
import Form.ContactDetails as ContactForm
import Form.RetentionPolicy as RetentionPolicy

import Validate exposing (Validator, Valid)
import Validate as V
import Validate.Extra as V

---- MODEL ----


type alias Model =
    { userGroup : Result Json.Error UserGroup
    , tagInProgress : TagsForm.TagInProgress
    , errors : List Form.Error
    , viewMode : ViewJson -- TODO: remove, only for debugging
    }


init : ( Model, Cmd Msg )
init =
    (
        { userGroup = Json.decodeString UG.decoder Data.userGroup
        , tagInProgress = TagsForm.none
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
    | TryCreateTag { newName : String, newValue : String }
    | TryRestoreTag TagToRemove { newValue : String }
    | TryChangeTag Tag { newValue : String }
    | TryRemoveTag Tag
    | TryUpdateAddress CD.Address
    | ToggleJsonMode ViewJson



update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    ( case msg of

        TagInProgress inProgress ->
            { model | tagInProgress = inProgress }

        TryCreateTag { newName, newValue } ->
            validateTagAnd addTag (tagValidator { checkUnique = True }) model <| Tag.make newName newValue

        TryChangeTag theTag { newValue } ->
            validateTagAnd changeTag (tagValidator { checkUnique = False }) model <| Tag.setValue newValue theTag

        TryRestoreTag ttr { newValue } ->
            validateTagAnd restoreTag (tagValidator { checkUnique = False }) model <| Tag.make (Tag.nameOfRemoved ttr) newValue

        TryRemoveTag theTag ->

            { model
            | tagInProgress = TagsForm.none
            , userGroup =
                Result.map
                    (\ug -> { ug | tags = removeTag theTag ug.tags })
                    model.userGroup
            }

        TryUpdateAddress newAddress ->
            let
                resValidAddress = V.validate addressValidator newAddress
            in
                case resValidAddress of
                    Ok validAddress ->
                        { model | userGroup = Result.map (updateAddress validAddress) model.userGroup }
                    Err errors ->
                        { model | errors = errors }

        ToggleJsonMode jsonMode ->
            { model | viewMode = jsonMode }


        NoOp -> model

    , Cmd.none
    )


validateTagAnd : (Valid Tag -> UG.TagList -> UG.TagList) -> (UG.TagList -> Validator Form.Error Tag) -> Model -> Tag -> Model
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


addTag : Valid Tag -> UG.TagList -> UG.TagList
addTag validTag list =
    -- V.fromValid >> Right >> (::) -- adds to the start of list instead of the end
    list ++ [ Right <| V.fromValid validTag ]


changeTag : Valid Tag -> UG.TagList -> UG.TagList
changeTag validTag =
    let theTag = V.fromValid validTag
    in List.map <| Either.mapRight
        (\otherTag ->
            if Tag.nameOf otherTag == Tag.nameOf theTag then theTag else otherTag
        )


removeTag : Tag -> UG.TagList -> UG.TagList
removeTag theTag =
    List.map
        <| Either.andThenRight
            (\otherTag ->
                if Tag.nameOf otherTag == Tag.nameOf theTag then Left <| Tag.toRemoved theTag else Right otherTag
            )


restoreTag : {- TagToRemove -> -} Valid Tag -> UG.TagList -> UG.TagList
restoreTag validTag =
    let theTag = V.fromValid validTag
    in List.map
        <| Either.andThenLeft
            (\removedTag ->
                if Tag.nameOfRemoved removedTag == Tag.nameOf theTag then Right theTag else Left removedTag
            )


tagValidator : { checkUnique : Bool } -> UG.TagList -> Validator Form.Error Tag
tagValidator { checkUnique } currentTags =
    Validate.all
        [ V.ifLongerThan Tag.nameOf 32 <| FE.make FE.NewTagName "Tag name should not exceed 32 characters"
        ,
            if checkUnique then
                let
                    tagNames = Set.fromList <| List.map (Either.unpack Tag.nameOfRemoved Tag.nameOf) currentTags
                in
                    V.ifNotUnique Tag.nameOf tagNames <| FE.make FE.NewTagName "One of tags already has this name, please try another one"
            else V.skip

        , V.ifBlank Tag.nameOf  <| FE.make FE.NewTagName "Tag name should not be empty"
        , V.ifBlank Tag.valueOf <| FE.make FE.NewTagValue "Value should not be empty"
        ]


addressValidator : Validator Form.Error CD.Address
addressValidator = V.skip


updateAddress : Valid CD.Address -> UG.UserGroup -> UG.UserGroup
updateAddress nextAddress ugroup =
    let
        curDetails = ugroup.contactDetails
    in
        { ugroup | contactDetails = { curDetails | address = V.fromValid nextAddress } }



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
            { tryCreate  = TryCreateTag
            , tryChange  = TryChangeTag
            , tryRestore = TryRestoreTag
            , tryRemove  = TryRemoveTag
            , markInProgress = TagInProgress
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
                    , ContactForm.view { readOnly = False, toMsg = TryUpdateAddress } userGroup.contactDetails
                    , Html.h1 [] [ Html.text "Tags" ]
                    , TagsForm.view tagHandlers model.tagInProgress userGroup.tags
                    , Html.h1 [] [ Html.text "Settings" ]
                    , RetentionPolicy.view <| RP.toList userGroup.settings.policy
                    ]
                Err error ->
                    [ Html.text <| Json.errorToString error ]
            ) ++
            [ Html.div [] <| List.map (FE.textOf >> Html.text) model.errors
            , Html.div
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
