module AddTagForm exposing (view)

import Button
import Html exposing (Html)
import Html.Attributes as Attributes
import Html.Events as Events
import Html.Extra
import Icon
import Types exposing (Tag)


type alias FormModel msg =
    { newTag : Maybe Tag
    , formErrorMessage : Maybe String
    , newTagNameInputMsg : String -> msg
    , newTagValueInputMsg : String -> msg
    , addTagMsg : msg
    , cancelMsg : msg
    }


requiredAsteriskView : Html msg
requiredAsteriskView =
    Html.span [ Attributes.class "text-red-500", Attributes.attribute "aria-hidden" "true" ] [ Html.text "\u{200A}*\u{00A0}" ]


errorMessageView : String -> Html msg
errorMessageView errorMessage =
    Html.div [ Attributes.class "p-3 bg-rose-100 border border-rose-400 text-rose-700 rounded" ] [ Html.text errorMessage ]


view : FormModel msg -> Html msg
view { newTag, formErrorMessage, newTagNameInputMsg, newTagValueInputMsg, addTagMsg, cancelMsg } =
    Html.section [ Attributes.class "w-full max-w-lg mx-auto flex flex-col gap-4 p-4 border border-gray-200 rounded" ]
        [ Html.h3 [ Attributes.class "text-lg font-bold" ] [ Html.text "Add a new tag" ]
        , Html.p [ Attributes.class "text-sm text-gray-600" ] [ Html.text "* Required fields" ]
        , Html.Extra.viewMaybe errorMessageView formErrorMessage
        , Html.form
            [ Attributes.id "add-tag-form"
            , Attributes.class "flex flex-col gap-4"
            , Events.onSubmit addTagMsg
            ]
            [ Html.label [ Attributes.class "flex flex-col gap-1 text-sm font-medium" ]
                [ Html.p []
                    [ Html.text "Tag name"
                    , requiredAsteriskView
                    ]
                , Html.input
                    [ Attributes.type_ "text"
                    , Attributes.id "tag-name-input"
                    , Attributes.required True
                    , Attributes.placeholder "Enter name, e.g. 'Powered by'"
                    , Attributes.maxlength 32
                    , Attributes.attribute "aria-describedby" "tag-name-max-length"
                    , Attributes.class "border border-gray-300 rounded px-3 py-2"
                    , newTag
                        |> Maybe.map (\{ name } -> name)
                        |> Maybe.withDefault ""
                        |> Attributes.value
                    , Events.onInput newTagNameInputMsg
                    ]
                    []
                , Html.p [ Attributes.id "tag-name-max-length", Attributes.class "text-xs text-gray-600" ] [ Html.text "Maximum length is 32 characters." ]
                ]
            , Html.label [ Attributes.class "flex flex-col gap-1 text-sm font-medium" ]
                [ Html.text "Tag value"
                , Html.input
                    [ Attributes.type_ "text"
                    , Attributes.id "tag-value-input"
                    , Attributes.placeholder "Enter value, e.g. 'elm'"
                    , Attributes.class "border border-gray-300 rounded px-3 py-2"
                    , newTag
                        |> Maybe.andThen .value
                        |> Maybe.withDefault ""
                        |> Attributes.value
                    , Events.onInput newTagValueInputMsg
                    ]
                    []
                ]
            , Html.div [ Attributes.class "flex flex-col sm:flex-row sm:items-center sm:justify-end gap-3" ]
                [ Button.new
                    { class = "text-gray-900 font-bold py-2 px-4 rounded border border-gray-900"
                    , label = "Cancel"
                    }
                    |> Button.withAriaLabel "Cancel adding a new tag"
                    |> Button.withOnClick cancelMsg
                    |> Button.view
                , Button.new
                    { class = "bg-gray-900 hover:bg-gray-700 text-white font-bold py-2 px-4 rounded border border-gray-800 transition"
                    , label = "Add a new tag"
                    }
                    |> Button.withAriaLabel "Confirm adding a new tag"
                    |> Button.withType "submit"
                    |> Button.withIconLeft (Just Icon.saveIcon)
                    |> Button.view
                ]
            ]
        ]
