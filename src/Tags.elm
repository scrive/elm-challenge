module Tags exposing
    ( Model
    , Msg
    , decode
    , update
    , view
    )

import Form.Tag as Tag
import Html exposing (Html)
import Json.Decode as Decode exposing (Decoder)
import List.Extra as ListExtra


type Model
    = Model ModelData


type alias ModelData =
    { tags : List Tag.Tag
    , addTagFormModel : Tag.Model
    , editTagFormModel : Maybe Tag.Model
    }


initModel : List Tag.Tag -> Model
initModel tags =
    Model
        { tags = tags
        , addTagFormModel = Tag.initAddFormModel
        , editTagFormModel = Nothing
        }


decode : Decoder Model
decode =
    Decode.map initModel (Decode.list Tag.decodeTag)


type Msg
    = ClickedRemoveTag String
    | ClickedEditTag Tag.Tag
    | FormMsg Tag.FormType Tag.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg ((Model ({ addTagFormModel, editTagFormModel, tags } as modelData)) as model) =
    case msg of
        ClickedRemoveTag tagName ->
            let
                updatedTags =
                    tags
                        |> List.filter (\{ name } -> name /= tagName)
            in
            ( Model { modelData | tags = updatedTags }
            , Cmd.none
            )

        ClickedEditTag tag ->
            let
                editTagFormModel_ =
                    tag
                        |> Tag.initEditFormModel
                        |> Just
            in
            ( Model { modelData | editTagFormModel = editTagFormModel_ }
            , Cmd.none
            )

        FormMsg Tag.Add formMsg ->
            let
                ( updatedAddTagFormModel, tagCmd, tagParentMsg ) =
                    Tag.update tags formMsg addTagFormModel

                mappedCmd =
                    Cmd.map (FormMsg Tag.Add) tagCmd

                updatedModelData =
                    { modelData | addTagFormModel = updatedAddTagFormModel }

                updatedModelWithCmd =
                    ( Model updatedModelData
                    , mappedCmd
                    )
            in
            case tagParentMsg of
                Tag.NoUpdate ->
                    updatedModelWithCmd

                Tag.HideEditForm ->
                    updatedModelWithCmd

                Tag.SubmittedForm submittedTag ->
                    ( Model { updatedModelData | tags = tags ++ [ submittedTag ] }
                    , mappedCmd
                    )

        FormMsg Tag.Edit formMsg ->
            editTagFormModel
                |> Maybe.map
                    (\editTagFormModel_ ->
                        let
                            ( updatedEditTagFormModel, tagCmd, tagParentMsg ) =
                                Tag.update tags formMsg editTagFormModel_

                            mappedCmd =
                                Cmd.map (FormMsg Tag.Edit) tagCmd

                            modelDataHiddenEditForm =
                                { modelData | editTagFormModel = Nothing }
                        in
                        case tagParentMsg of
                            Tag.NoUpdate ->
                                ( Model { modelData | editTagFormModel = Just updatedEditTagFormModel }
                                , mappedCmd
                                )

                            Tag.HideEditForm ->
                                ( Model modelDataHiddenEditForm
                                , mappedCmd
                                )

                            Tag.SubmittedForm submittedTag ->
                                ( Model { modelDataHiddenEditForm | tags = updateTagInList submittedTag tags }
                                , mappedCmd
                                )
                    )
                |> Maybe.withDefault
                    ( model
                    , Cmd.none
                    )



-- let
--     ( updatedAddTagFormModel, tagCmd, tagParentMsg ) =
--         Tag.update formMsg addTagFormModel
--     updatedModelWithCmd =
--         ( Model { model | addTagFormModel = updatedAddTagFormModel }
--         , Cmd.map tagCmd FormMsg
--         )
-- in
-- case tagParentMsg of
--     Tag.NoUpdate ->
--         updatedModelWithCmd
--     Tag.HideEditForm ->
--         updatedModelWithCmd
--     Tag.SubmittedForm submittedTag ->
--         ( Model { model | tags = updateTagInList tag tags }
--         , Cmd.map tagCmd FormMsg
--         )


updateTagInList : Tag.Tag -> List Tag.Tag -> List Tag.Tag
updateTagInList updatedTag =
    ListExtra.updateIf (\tag -> tag.name == updatedTag.name) identity



-- FormMsg Edit formMsg ->


view : Model -> Html Msg
view (Model model) =
    Html.text "Tags view"
