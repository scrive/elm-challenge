module Section.Tags exposing
    ( Model
    , Msg
    , init
    , update
    , view
    )

import Data.Tag as Tag
import Form.Tag as TagForm
import Html exposing (Html)
import List.Extra as ListExtra


type Model
    = Model ModelData


type alias ModelData =
    { addTagFormModel : TagForm.Model
    , editTagFormModel : Maybe TagForm.Model
    }


init : Model
init =
    Model
        { addTagFormModel = TagForm.initAddFormModel
        , editTagFormModel = Nothing
        }


type Msg
    = ClickedRemoveTag Tag.Model
    | ClickedEditTag Tag.Model
    | FormMsg TagForm.FormType TagForm.Msg


update : Tag.Tags -> Msg -> Model -> ( Model, Cmd Msg, Tag.Tags )
update tags msg ((Model ({ addTagFormModel, editTagFormModel } as modelData)) as model) =
    case msg of
        ClickedRemoveTag tag ->
            let
                updatedTags =
                    List.filter (Tag.isSameTag tag >> not) tags
            in
            ( model
            , Cmd.none
            , updatedTags
            )

        ClickedEditTag tag ->
            let
                editTagFormModel_ =
                    tag
                        |> TagForm.initEditFormModel
                        |> Just
            in
            ( Model { modelData | editTagFormModel = editTagFormModel_ }
            , Cmd.none
            , tags
            )

        FormMsg TagForm.Add formMsg ->
            let
                ( updatedAddTagFormModel, tagCmd, tagParentMsg ) =
                    TagForm.update formMsg addTagFormModel

                mappedCmd =
                    Cmd.map (FormMsg TagForm.Add) tagCmd

                updatedModelData =
                    { modelData | addTagFormModel = updatedAddTagFormModel }

                updatedModelWithCmd =
                    ( Model updatedModelData
                    , mappedCmd
                    , tags
                    )
            in
            case tagParentMsg of
                TagForm.NoUpdate ->
                    updatedModelWithCmd

                -- Never happen
                TagForm.HideEditForm ->
                    updatedModelWithCmd

                TagForm.SubmittedForm submittedTag ->
                    ( model
                    , mappedCmd
                    , tags ++ [ submittedTag ]
                    )

        FormMsg TagForm.Edit formMsg ->
            editTagFormModel
                |> Maybe.map
                    (\editTagFormModel_ ->
                        let
                            ( updatedEditTagFormModel, tagCmd, tagParentMsg ) =
                                TagForm.update formMsg editTagFormModel_

                            mappedCmd =
                                Cmd.map (FormMsg TagForm.Edit) tagCmd

                            hiddenEditTagFormModel =
                                Model { modelData | editTagFormModel = Nothing }
                        in
                        case tagParentMsg of
                            TagForm.NoUpdate ->
                                ( Model { modelData | editTagFormModel = Just updatedEditTagFormModel }
                                , mappedCmd
                                , tags
                                )

                            TagForm.HideEditForm ->
                                ( hiddenEditTagFormModel
                                , mappedCmd
                                , tags
                                )

                            TagForm.SubmittedForm submittedTag ->
                                ( model
                                , mappedCmd
                                , updateTagInList submittedTag tags
                                )
                    )
                |> Maybe.withDefault
                    ( model
                    , Cmd.none
                    , tags
                    )


updateTagInList : Tag.Model -> Tag.Tags -> Tag.Tags
updateTagInList tag =
    ListExtra.updateIf (Tag.isSameTag tag) (always tag)



-- FormMsg Edit formMsg ->


view : Tag.Tags -> Model -> Html Msg
view tags (Model model) =
    Html.text "Tags view"
