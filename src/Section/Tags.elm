module Section.Tags exposing
    ( Model
    , Msg
    , init
    , update
    , view
    )

import Data.Tag as Tag
import Form.Tag as FormTag
import Html exposing (Html)
import Html.Attributes as Attributes
import Ui.Button as Button
import Ui.Modal as Modal


type Model
    = Model Forms


type alias Forms =
    { addTagFormModel : FormTag.Model
    , editTagFormModel : Maybe EditTagFormModel
    }


type alias EditTagFormModel =
    ( FormTag.Model
    , Tag.Model
    )


init : Model
init =
    Model
        { addTagFormModel = FormTag.initAddFormModel
        , editTagFormModel = Nothing
        }


type Msg
    = ClickedRemoveTag Tag.Model
    | ClickedEditTag Tag.Model
    | ClickedCloseEditTagForm
    | FormMsg FormTag.FormType FormTag.Msg


update : Tag.Tags -> Msg -> Model -> ( Model, Cmd Msg, Tag.Tags )
update tags msg ((Model ({ addTagFormModel, editTagFormModel } as modelData)) as model) =
    case msg of
        ClickedRemoveTag tag ->
            let
                updatedTags =
                    List.filter (Tag.isSameTagName tag >> not) tags
            in
            ( model
            , Cmd.none
            , updatedTags
            )

        ClickedEditTag tag ->
            ( Model { modelData | editTagFormModel = Just ( FormTag.initEditFormModel tag, tag ) }
            , Cmd.none
            , tags
            )

        ClickedCloseEditTagForm ->
            ( Model { modelData | editTagFormModel = Nothing }
            , Cmd.none
            , tags
            )

        FormMsg FormTag.Add formMsg ->
            let
                ( updatedAddTagFormModel, tagCmd, tagParentMsg ) =
                    FormTag.update tags formMsg addTagFormModel

                mappedCmd =
                    Cmd.map (FormMsg FormTag.Add) tagCmd

                updatedModel =
                    Model { modelData | addTagFormModel = updatedAddTagFormModel }

                updatedModelWithCmd =
                    ( updatedModel
                    , mappedCmd
                    , tags
                    )
            in
            case tagParentMsg of
                FormTag.NoUpdate ->
                    updatedModelWithCmd

                FormTag.SubmittedForm submittedTag ->
                    ( updatedModel
                    , mappedCmd
                    , tags ++ [ submittedTag ]
                    )

        FormMsg FormTag.Edit formMsg ->
            editTagFormModel
                |> Maybe.map
                    (\( editTagFormModel_, originalTag ) ->
                        let
                            ( updatedEditTagFormModel, tagCmd, tagParentMsg ) =
                                FormTag.update tags formMsg editTagFormModel_

                            mappedCmd =
                                Cmd.map (FormMsg FormTag.Edit) tagCmd

                            hiddenEditTagFormModel =
                                Model { modelData | editTagFormModel = Nothing }
                        in
                        case tagParentMsg of
                            FormTag.NoUpdate ->
                                ( Model { modelData | editTagFormModel = Just ( updatedEditTagFormModel, originalTag ) }
                                , mappedCmd
                                , tags
                                )

                            FormTag.SubmittedForm submittedTag ->
                                ( hiddenEditTagFormModel
                                , mappedCmd
                                , updateTagInList
                                    { originalTag = originalTag
                                    , updatedTag = submittedTag
                                    }
                                    tags
                                )
                    )
                |> Maybe.withDefault
                    ( model
                    , Cmd.none
                    , tags
                    )


updateTagInList : { originalTag : Tag.Model, updatedTag : Tag.Model } -> Tag.Tags -> Tag.Tags
updateTagInList { originalTag, updatedTag } =
    List.map
        (\tag ->
            if Tag.isSameTagName tag originalTag then
                updatedTag

            else
                tag
        )


view : Tag.Tags -> Model -> Html Msg
view tags (Model { addTagFormModel, editTagFormModel }) =
    Html.div [ Attributes.class "w-96" ]
        [ tagsView tags
        , Html.h1 [ Attributes.class "text-center mt-5" ] [ Html.text "Create new tag" ]
        , Html.div [ Attributes.class "mt-2" ]
            [ FormTag.view addTagFormModel
                |> Html.map (FormMsg FormTag.Add)
            ]
        , editTagFormModel
            |> Maybe.map modalView
            |> Maybe.withDefault (Html.text "")
        ]


modalView : EditTagFormModel -> Html Msg
modalView ( formTagModel, tag ) =
    formTagModel
        |> FormTag.view
        |> Html.map (FormMsg FormTag.Edit)
        |> Modal.view
            { title = formatTag tag
            , closeMsg = ClickedCloseEditTagForm
            }


tagsView : Tag.Tags -> Html Msg
tagsView =
    List.map tagView
        >> Html.ul [ Attributes.class "max-w-md space-y-1 text-gray-500 list-none list-inside dark:text-gray-400" ]


tagView : Tag.Model -> Html Msg
tagView tag =
    Html.li [ Attributes.class "flex justify-between" ]
        [ Html.text <| formatTag tag
        , Html.div []
            [ Button.view []
                { text = "Edit"
                , msg = ClickedEditTag tag
                , size = Button.extraSmall
                }
            , Button.view [ Attributes.class "ml-1" ]
                { text = "Remove"
                , msg = ClickedRemoveTag tag
                , size = Button.extraSmall
                }
            ]
        ]


formatTag : Tag.Model -> String
formatTag tag =
    let
        name =
            Tag.getName tag

        value =
            Tag.getValue tag
    in
    case value of
        "" ->
            name

        _ ->
            String.join " - " [ name, value ]
