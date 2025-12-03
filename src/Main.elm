module Main exposing (main)

import AddTagForm
import Browser
import Browser.Dom as Dom
import Button
import Data
import Html exposing (Html)
import Html.Attributes as Attributes
import Html.Events as Events
import Html.Extra
import Icon
import Json.Decode as Decode
import Json.Decode.Pipeline as Pipeline
import List.Extra
import Process
import Task
import Types exposing (Tag)



---- MODEL ----


type alias Model =
    { tags : List Tag
    , newTag : Maybe Tag
    , editingTagIndex : Maybe Int
    , editingTagValue : String
    , addTagError : Maybe String
    , shouldShowAddTagForm : Bool
    , ariaLiveMessage : Maybe String
    , pendingDeleteOnIndex : Maybe Int
    }


init : ( Model, Cmd Msg )
init =
    let
        decodeTag =
            Decode.succeed Tag
                |> Pipeline.required "name" Decode.string
                |> Pipeline.optional "value" (Decode.maybe Decode.string) Nothing

        decodedTags =
            Decode.decodeString (Decode.field "tags" (Decode.list decodeTag)) Data.userGroup
                |> Result.withDefault []
    in
    ( { tags = decodedTags
      , newTag = Nothing
      , editingTagIndex = Nothing
      , editingTagValue = ""
      , addTagError = Nothing
      , shouldShowAddTagForm = False
      , ariaLiveMessage =
            if List.isEmpty decodedTags then
                Just "No tags found on the user group. Create a new tag by clicking \"Add a new tag\"."

            else
                Nothing
      , pendingDeleteOnIndex = Nothing
      }
    , Cmd.none
    )



---- UPDATE ----


type Msg
    = NoOp
    | EditTag Int
    | DeleteTag Int
    | ConfirmDeleteTag Int
    | CancelPendingDelete Int
    | OpenAddForm
    | CloseAddForm
    | AddNewTag
    | InsertNewTagName String
    | InsertNewTagValue String
    | InsertEditTagValue String
    | SubmitEditTagValue Int


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        EditTag index ->
            case List.Extra.getAt index model.tags of
                Just tag ->
                    ( { model | editingTagIndex = Just index, editingTagValue = Maybe.withDefault "" tag.value, ariaLiveMessage = Just ("Editing tag " ++ tag.name) }
                    , Task.attempt (\_ -> NoOp) (Dom.focus "edit-tag-value-input")
                    )

                Nothing ->
                    ( model, Cmd.none )

        DeleteTag index ->
            ( { model
                | pendingDeleteOnIndex = Just index
                , ariaLiveMessage = Just "Click delete again to confirm"
              }
            , Task.perform (\_ -> CancelPendingDelete index)
                (Task.succeed ()
                    |> Task.andThen (always (Process.sleep 3000))
                )
            )

        ConfirmDeleteTag index ->
            let
                updatedModel =
                    { model
                        | tags = List.Extra.removeAt index model.tags
                        , ariaLiveMessage = Just "Tag deleted successfully"
                        , pendingDeleteOnIndex = Nothing
                    }
            in
            if Just index == model.editingTagIndex then
                ( { updatedModel | editingTagIndex = Nothing, editingTagValue = "" }, Cmd.none )

            else
                ( updatedModel, Cmd.none )

        CancelPendingDelete index ->
            if model.pendingDeleteOnIndex == Just index then
                ( { model | pendingDeleteOnIndex = Nothing }, Cmd.none )

            else
                ( model, Cmd.none )

        OpenAddForm ->
            ( { model | shouldShowAddTagForm = True, addTagError = Nothing, newTag = Nothing, ariaLiveMessage = Just "Opened \"Add a new tag\" form." }
            , Task.attempt (\_ -> NoOp) (Dom.focus "tag-name-input")
            )

        CloseAddForm ->
            ( { model | shouldShowAddTagForm = False, addTagError = Nothing, newTag = Nothing, ariaLiveMessage = Just "Closed \"Add a new tag\" form." }
            , Task.attempt (\_ -> NoOp) (Dom.focus "add-tag-button")
            )

        AddNewTag ->
            case model.newTag of
                Just newTag ->
                    let
                        trimmedNewTagName =
                            String.trim newTag.name

                        errorMessage =
                            if trimmedNewTagName == "" then
                                Just "Tag name cannot be empty"

                            else if List.any (\existingTag -> existingTag.name == trimmedNewTagName) model.tags then
                                Just "A tag with this name already exists"

                            else
                                Nothing
                    in
                    case errorMessage of
                        Just error ->
                            ( { model | addTagError = Just error, ariaLiveMessage = Just error }, Cmd.none )

                        Nothing ->
                            ( { model
                                | tags = { newTag | name = trimmedNewTagName } :: model.tags
                                , newTag = Nothing
                                , addTagError = Nothing
                                , shouldShowAddTagForm = False
                                , ariaLiveMessage = Just "Tag added successfully"
                              }
                            , Cmd.none
                            )

                Nothing ->
                    let
                        error =
                            "Tag name cannot be empty"
                    in
                    ( { model | addTagError = Just error, ariaLiveMessage = Just error }, Cmd.none )

        InsertNewTagName name ->
            case model.newTag of
                Just tag ->
                    ( { model | newTag = Just { tag | name = name }, addTagError = Nothing }, Cmd.none )

                Nothing ->
                    ( { model | newTag = Just { name = name, value = Just "" }, addTagError = Nothing }, Cmd.none )

        InsertNewTagValue value ->
            case model.newTag of
                Just tag ->
                    ( { model | newTag = Just { tag | value = Just value } }, Cmd.none )

                Nothing ->
                    ( { model | newTag = Just { name = "", value = Just value } }, Cmd.none )

        InsertEditTagValue value ->
            ( { model | editingTagValue = value }, Cmd.none )

        SubmitEditTagValue index ->
            ( { model
                | tags = List.Extra.updateAt index (\tag -> { tag | value = Just model.editingTagValue }) model.tags
                , editingTagIndex = Nothing
                , editingTagValue = ""
                , ariaLiveMessage = Just "Tag value updated successfully"
              }
            , Cmd.none
            )



---- VIEW ----


view : Model -> Html Msg
view model =
    Html.main_ [ Attributes.class "flex flex-col w-full max-w-4xl mx-auto px-4 mt-16 mb-48" ]
        [ ariaLiveRegion model.ariaLiveMessage
        , Html.h1 [ Attributes.class "text-3xl font-bold mb-8" ] [ Html.text "Tags manager" ]
        , formView model
        ]


formView : Model -> Html Msg
formView { tags, newTag, editingTagIndex, editingTagValue, addTagError, shouldShowAddTagForm, pendingDeleteOnIndex } =
    let
        isBeingEdited tagIndex =
            case editingTagIndex of
                Just editingIndex ->
                    tagIndex == editingIndex

                Nothing ->
                    False
    in
    Html.section [ Attributes.class "flex flex-col w-full gap-3" ]
        [ Html.h2 [ Attributes.class "text-xl font-bold" ] [ Html.text "Tags" ]
        , Html.div [ Attributes.class "w-full" ]
            [ if List.isEmpty tags then
                Html.p
                    [ Attributes.class "text-gray-500 italic text-center py-6" ]
                    [ Html.text "No tags found on the user group. Create a new tag by clicking \"Add a new tag\"." ]

              else
                Html.div []
                    [ Html.div
                        [ Attributes.class "grid grid-cols-12 font-semibold text-gray-600 text-sm border-b pb-2 mb-2"
                        , Attributes.attribute "aria-hidden" "true"
                        ]
                        [ Html.div [ Attributes.class "col-span-5 font-bold text-left" ] [ Html.text "Name" ]
                        , Html.div [ Attributes.class "col-span-4 font-bold text-center" ] [ Html.text "Value" ]
                        , Html.div [ Attributes.class "col-span-3 font-bold text-right" ] [ Html.text "Actions" ]
                        ]
                    , Html.ul [ Attributes.class "divide-y divide-gray-200", Attributes.attribute "role" "list", Attributes.attribute "aria-label" "List of tags, with columns Name, Value, and Actions" ] (List.indexedMap (\index tag -> tagView index tag (isBeingEdited index) editingTagValue pendingDeleteOnIndex) tags)
                    ]
            ]
        , if shouldShowAddTagForm then
            AddTagForm.view
                { newTag = newTag
                , formErrorMessage = addTagError
                , newTagNameInputMsg = InsertNewTagName
                , newTagValueInputMsg = InsertNewTagValue
                , addTagMsg = AddNewTag
                , cancelMsg = CloseAddForm
                }

          else
            Html.div [ Attributes.class "w-full max-w-lg mx-auto flex justify-center" ]
                [ Button.new
                    { class = "bg-gray-900 hover:bg-gray-700 text-white font-bold py-2 px-4 rounded border border-gray-800 transition"
                    , label = "Add a new tag"
                    }
                    |> Button.withId "add-tag-button"
                    |> Button.withOnClick OpenAddForm
                    |> Button.view
                ]
        ]


tagView : Int -> Tag -> Bool -> String -> Maybe Int -> Html Msg
tagView index { name, value } isBeingEdited editingValue pendingDeleteIndex =
    Html.li [ Attributes.class "grid grid-cols-12 items-center bg-white even:bg-gray-50 p-2 sm:p-3 rounded-md shadow-sm", Attributes.attribute "role" "listitem" ]
        [ Html.div [ Attributes.class "col-span-5 flex items-center" ] [ Html.span [ Attributes.class "text-slate-800 text-sm sm:text-base break-all" ] [ Html.text name ] ]
        , if isBeingEdited then
            Html.form
                [ Attributes.id "edit-tag-value-form"
                , Attributes.class "col-span-4 flex flex-col gap-2"
                , Events.onSubmit (SubmitEditTagValue index)
                ]
                [ Html.label [ Attributes.class "flex flex-col gap-1 text-sm font-medium" ]
                    [ Html.text "Tag value"
                    , Html.input
                        [ Attributes.type_ "text"
                        , Attributes.id "edit-tag-value-input"
                        , Attributes.placeholder "Enter value, e.g. 'elm'"
                        , Attributes.value editingValue
                        , Attributes.class "border border-gray-300 rounded px-2 py-1 text-sm w-full"
                        , Events.onInput InsertEditTagValue
                        ]
                        []
                    ]
                ]

          else
            Html.div [ Attributes.class "col-span-4 flex items-center justify-center" ]
                [ Html.Extra.viewMaybe
                    (\tagValue ->
                        Html.span [ Attributes.class "text-center text-slate-800 text-sm sm:text-base break-all" ] [ Html.text tagValue ]
                    )
                    value
                ]
        , Html.div [ Attributes.class "col-span-3 flex flex-col sm:flex-row gap-1 sm:gap-3 justify-end items-end" ]
            [ if isBeingEdited then
                Button.new
                    { class = "bg-gray-900 hover:bg-gray-700 text-white font-bold py-1 px-2 sm:py-2 sm:px-4 rounded text-xs sm:text-sm"
                    , label = ""
                    }
                    |> Button.withAriaLabel ("Save edited value for tag " ++ name)
                    |> Button.withType "submit"
                    |> Button.withFormId "edit-tag-value-form"
                    |> Button.withIconLeft (Just Icon.saveIcon)
                    |> Button.view

              else
                Button.new
                    { class = "bg-gray-900 hover:bg-gray-700 text-white font-bold py-1 px-2 sm:py-2 sm:px-4 rounded border border-gray-800 transition text-xs sm:text-sm"
                    , label = ""
                    }
                    |> Button.withAriaLabel ("Edit tag " ++ name)
                    |> Button.withOnClick (EditTag index)
                    |> Button.withIconLeft (Just Icon.pencilIcon)
                    |> Button.view
            , if pendingDeleteIndex == Just index then
                Button.new
                    { class = "bg-red-600 text-white font-bold py-1 px-2 sm:py-2 sm:px-4 rounded border border-red-700 hover:bg-red-700 transition text-xs sm:text-sm animate-pulse"
                    , label = "Confirm delete"
                    }
                    |> Button.withAriaLabel ("Click again to confirm deleting tag " ++ name)
                    |> Button.withOnClick (ConfirmDeleteTag index)
                    |> Button.view

              else
                Button.new
                    { class = "bg-rose-50 text-rose-700 font-bold py-1 px-2 sm:py-2 sm:px-4 rounded border border-rose-700 hover:bg-rose-100 transition text-xs sm:text-sm"
                    , label = ""
                    }
                    |> Button.withAriaLabel ("Delete tag " ++ name)
                    |> Button.withOnClick (DeleteTag index)
                    |> Button.withIconLeft (Just Icon.trashIcon)
                    |> Button.view
            ]
        ]


ariaLiveRegion : Maybe String -> Html Msg
ariaLiveRegion maybeMessage =
    Html.div
        [ Attributes.class "sr-only"
        , Attributes.attribute "role" "status"
        , Attributes.attribute "aria-live" "polite"
        , Attributes.attribute "aria-atomic" "true"
        ]
        [ Html.text (Maybe.withDefault "" maybeMessage) ]



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
