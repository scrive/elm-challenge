module Main exposing (main)

import Browser
import Data
import Html exposing (Html)
import Html.Attributes as Attrs
import Html.Attributes.Extra as AttrsExtra
import Html.Events as Events
import Html.Extra
import Json.Decode as Decode
import Json.Decode.Pipeline as Pipeline
import Shared.Button as Button
import Shared.Editable as Editable
import Shared.Icon as Icon
import Shared.Value as Value


type alias Tag =
    { name : Editable.Editable String
    , value : Editable.Editable String
    }


type alias Model =
    { tags : List Tag
    , newTagForm : Tag
    , globalError : Maybe String
    }


init : ( Model, Cmd Msg )
init =
    let
        ( tags, error ) =
            decodeTagsWithMaybeError
    in
    ( { tags = tags, newTagForm = emptyNewTagForm, globalError = error }, Cmd.none )


emptyNewTagForm : Tag
emptyNewTagForm =
    { name = Value.validValue "" |> Editable.valueToEditable
    , value = Value.validValue "" |> Editable.valueToEditable
    }


tagDecoder : Decode.Decoder Tag
tagDecoder =
    Decode.succeed Tag
        |> Pipeline.required "name" (Decode.map Editable.toReadonly Decode.string)
        |> Pipeline.optional "value" (Decode.map Editable.toReadonly Decode.string) (Editable.toReadonly "")


decodeTagsWithMaybeError : ( List Tag, Maybe String )
decodeTagsWithMaybeError =
    let
        decodeTags =
            Decode.field "tags" (Decode.list tagDecoder)
    in
    case Decode.decodeString decodeTags Data.userGroup of
        Ok decodedTags ->
            ( decodedTags, Nothing )

        Err decodeError ->
            ( [], Just ("Error decoding data: " ++ Decode.errorToString decodeError) )


type Msg
    = UpdateNewName String
    | UpdateNewValue String
    | ClickedAddTag
    | ClickedRemoveTag String
    | InsertedTagValue String String
    | ClickedEditTagValue Bool String
    | ClickedReloadData
    | NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UpdateNewName nameString ->
            let
                updateName tag =
                    { tag | name = Editable.updateIfEditable nameString tag.name }
            in
            ( { model | newTagForm = updateName model.newTagForm }, Cmd.none )

        UpdateNewValue valueString ->
            let
                updateValue tag =
                    { tag | value = Editable.updateIfEditable valueString tag.value }
            in
            ( { model | newTagForm = updateValue model.newTagForm }, Cmd.none )

        ClickedAddTag ->
            let
                validatedName =
                    Editable.valueFromEditable model.newTagForm.name |> validateNameValue model.tags
            in
            if Value.isValid validatedName then
                ( { model
                    | tags =
                        model.tags
                            ++ [ { name = Value.fromValue validatedName |> Editable.toReadonly
                                 , value = model.newTagForm.value |> Editable.setReadonly
                                 }
                               ]
                    , newTagForm = emptyNewTagForm
                  }
                , Cmd.none
                )

            else
                ( { model
                    | newTagForm =
                        { name = Editable.valueToEditable validatedName
                        , value = model.newTagForm.value
                        }
                  }
                , Cmd.none
                )

        ClickedRemoveTag nameToRemove ->
            let
                updatedTags =
                    List.filter (.name >> Editable.valueFromEditable >> Value.fromValue >> (/=) nameToRemove) model.tags

                validatedName =
                    Editable.valueFromEditable model.newTagForm.name |> validateNameValue updatedTags

                updateName tag =
                    { tag | name = Editable.valueToEditable validatedName }
            in
            ( { model
                | tags = updatedTags
                , newTagForm = updateName model.newTagForm
              }
            , Cmd.none
            )

        InsertedTagValue nameString newValue ->
            let
                updatedTags =
                    List.map
                        (\tag ->
                            if (Editable.valueFromEditable tag.name |> Value.fromValue) == nameString then
                                { tag | value = Editable.updateIfEditable newValue tag.value }

                            else
                                tag
                        )
                        model.tags
            in
            ( { model | tags = updatedTags }, Cmd.none )

        ClickedEditTagValue isEditable tagName ->
            let
                updatedTags =
                    List.map
                        (\tag ->
                            if (Editable.valueFromEditable tag.name |> Value.fromValue) == tagName then
                                { tag
                                    | value =
                                        if isEditable then
                                            Editable.setEditable tag.value

                                        else
                                            Editable.setReadonly tag.value
                                }

                            else
                                tag
                        )
                        model.tags
            in
            ( { model | tags = updatedTags }, Cmd.none )

        ClickedReloadData ->
            let
                ( tags, error ) =
                    decodeTagsWithMaybeError
            in
            ( { tags = tags, newTagForm = emptyNewTagForm, globalError = error }, Cmd.none )

        NoOp ->
            ( model, Cmd.none )


validateNameValue : List Tag -> Value.Value String -> Value.Value String
validateNameValue tags nameValue =
    let
        nameString =
            Value.fromValue nameValue
    in
    if String.isEmpty nameString then
        Value.addError "Name is required" nameValue

    else if List.any (.name >> Editable.valueFromEditable >> Value.fromValue >> (==) nameString) tags then
        Value.addError "Duplicate name" nameValue

    else
        Value.validValue nameString


header : String -> Html msg
header text =
    Html.span [ Attrs.class "p-2 text-5xl font-extrabold text-transparent bg-clip-text bg-gradient-to-br from-[#0052CC] to-[#003366]" ]
        [ Html.text text ]


subheader : String -> Html msg
subheader text =
    Html.span [ Attrs.class "p-2 text-2xl font-extrabold text-[#1A1A1A]" ]
        [ Html.text text ]


view : Model -> Html Msg
view model =
    let
        newTagFormView =
            Html.form
                [ Attrs.class "mb-6 grid grid-cols-1 md:grid-cols-[2fr_2fr_auto] gap-3 items-center w-full max-w-full"
                , Events.preventDefaultOn "submit" (Decode.succeed ( ClickedAddTag, True ))
                ]
                [ inputField "Tag Name *" model.newTagForm.name UpdateNewName
                , inputField "Tag Value" model.newTagForm.value UpdateNewValue
                , Button.new (Button.OnSubmit ClickedAddTag)
                    |> Button.withClass "bg-blue-600 text-white rounded-full shadow hover:bg-blue-700 transition w-10 h-10 flex items-center justify-center focus:outline-none focus:ring-2 focus:ring-blue-400"
                    |> Button.withIcon Icon.Add
                    |> Button.withTooltip "Add new tag"
                    |> Button.view
                ]
    in
    Html.div [ Attrs.class "min-h-screen bg-gray-50 py-16 px-6 flex flex-col items-center w-full max-w-full" ]
        (([ header "Tags form"
          , subheader "Form for tags management"
          ]
            ++ globalErrorView model.globalError
         )
            ++ [ Html.div
                    [ Attrs.class "w-full max-w-lg bg-white p-6 rounded-xl shadow-md border border-gray-200 mt-8" ]
                    [ refreshButtonView
                    , newTagFormView
                    , [ List.map tagView model.tags
                            |> Html.div [ Attrs.class "space-y-3", Attrs.attribute "role" "list", Attrs.attribute "aria-label" "Tags list" ]
                      ]
                        |> Html.div [ Attrs.class "max-h-[60vh] overflow-y-auto p-5" ]
                    ]
               ]
        )


globalErrorView : Maybe String -> List (Html msg)
globalErrorView =
    Maybe.map
        (Html.text
            >> List.singleton
            >> Html.div
                [ Attrs.class "text-red-600 mb-4 font-medium"
                , Attrs.attribute "role" "alert"
                , Attrs.attribute "aria-live" "assertive"
                ]
            >> List.singleton
        )
        >> Maybe.withDefault []


refreshButtonView : Html Msg
refreshButtonView =
    Html.div [ Attrs.class "flex justify-end py-4" ]
        [ Button.new (Button.OnClick ClickedReloadData)
            |> Button.withClass "ml-4 p-2 rounded-full bg-gray-200 hover:bg-gray-300 transition"
            |> Button.withIcon Icon.Refresh
            |> Button.withTooltip "Reload data"
            |> Button.view
        ]


tagView : Tag -> Html Msg
tagView tag =
    let
        valueValue =
            Editable.valueFromEditable tag.value |> Value.fromValue

        nameValue =
            Editable.valueFromEditable tag.name |> Value.fromValue
    in
    Html.div
        [ Attrs.class "flex items-start gap-3 bg-white p-3 rounded-lg border border-gray-200 shadow-sm min-h-[52px] box-border"
        , Attrs.attribute "role" "listitem"
        ]
        [ Html.div
            [ Attrs.class "flex-1 px-3 py-1 flex items-start text-gray-800 whitespace-pre-wrap break-all border border-transparent box-border"
            , Attrs.attribute "aria-label" ("Name for tag " ++ nameValue)
            ]
            [ Html.text nameValue ]
        , Html.div [ Attrs.class "flex-1 flex items-start gap-2 box-border" ]
            (if Editable.isEditable tag.value then
                [ Html.input
                    [ Attrs.value valueValue
                    , Events.onInput (InsertedTagValue nameValue)
                    , Attrs.class "border border-gray-300 rounded-md px-3 w-full h-9 focus:ring-blue-400 focus:border-blue-400 box-border"
                    , Attrs.attribute "aria-label" ("Value for tag " ++ nameValue)
                    ]
                    []
                , Html.div [ Attrs.class "relative flex items-start h-9 box-border" ]
                    [ Button.new (ClickedEditTagValue False nameValue |> Button.OnClick)
                        |> Button.withClass "w-8 h-8 flex items-center justify-center bg-gray-400 text-white rounded-md hover:bg-green-600 transition"
                        |> Button.withIcon Icon.Checkmark
                        |> Button.withTooltip "Confirm edit for tag"
                        |> Button.view
                    ]
                ]

             else
                [ Html.div
                    [ Attrs.class "w-full px-3 py-1 flex items-center text-gray-800 whitespace-pre-wrap break-all border border-transparent box-border"
                    , Attrs.attribute "aria-label" ("Value for tag " ++ nameValue)
                    ]
                    [ Html.text valueValue ]
                , Html.div [ Attrs.class "relative flex items-start h-9 box-border" ]
                    [ Button.new (ClickedEditTagValue True nameValue |> Button.OnClick)
                        |> Button.withClass "w-8 h-8 flex items-center justify-center bg-gray-400 text-white rounded-md hover:bg-yellow-500 transition"
                        |> Button.withIcon Icon.Edit
                        |> Button.withTooltip "Edit tag value"
                        |> Button.view
                    ]
                ]
            )
        , Html.div [ Attrs.class "relative flex items-start h-9 box-border" ]
            [ Button.new (ClickedRemoveTag nameValue |> Button.OnClick)
                |> Button.withClass "w-8 h-8 flex items-center justify-center bg-gray-400 text-white rounded-md hover:bg-red-600 transition"
                |> Button.withIcon Icon.Remove
                |> Button.withTooltip "Remove tag"
                |> Button.view
            ]
        ]


inputField : String -> Editable.Editable String -> (String -> Msg) -> Html Msg
inputField labelText editable toMsg =
    let
        valueString =
            Editable.valueFromEditable editable |> Value.fromValue

        errorText =
            Editable.valueFromEditable editable |> Value.getMaybeError

        borderClass =
            case errorText of
                Just _ ->
                    "border-red-500 focus:ring-red-400 focus:border-red-400"

                Nothing ->
                    "border-gray-300 focus:ring-blue-400 focus:border-blue-400"

        wrapperClass =
            "relative w-full"

        controlBoxClass =
            "block w-full h-11 rounded-md border px-3 text-sm text-gray-900 placeholder-gray-400 transition " ++ borderClass

        inputClass =
            "w-full h-full bg-transparent text-sm text-gray-900 focus:outline-none"
    in
    Html.div [ Attrs.class "w-full" ]
        [ Html.label
            [ Attrs.class "block text-gray-700 text-sm font-medium mb-1"
            , Attrs.attribute "for" labelText
            ]
            [ Html.text labelText ]
        , Html.div [ Attrs.class wrapperClass ]
            [ Html.div [ Attrs.class controlBoxClass ]
                [ Html.input
                    [ Attrs.id labelText
                    , Attrs.value valueString
                    , Events.onInput toMsg
                    , Attrs.class inputClass
                    , Attrs.maxlength 32
                    , Attrs.attribute "readonly" "true" |> AttrsExtra.attributeIf (not <| Editable.isEditable editable)
                    ]
                    []
                , Html.div
                    [ Attrs.class "pointer-events-none absolute inset-0 flex items-center px-3 text-gray-700" ]
                    [ Html.text valueString ]
                    |> Html.Extra.viewIf (Editable.isEditable editable |> not)
                ]
            ]
        , Html.p
            [ Attrs.class "text-red-600 text-xs mt-1 h-4" ]
            [ Html.text (Maybe.withDefault "" errorText) ]
        ]


main : Program () Model Msg
main =
    Browser.element
        { init = \_ -> init
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }
