module Update exposing (update)

import Data
import Data.Form as Form
import Data.UserGroup as UserGroup
import Json.Decode as JD
import Model exposing (Model, formOfModel, userGroupOfModel)
import Msg exposing (Msg(..))
import Update.Extra exposing (updateModel)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        LoadUserGroup ->
            ( model, Cmd.none )
                |> (case JD.decodeString UserGroup.decode Data.userGroup of
                        Ok userGroup ->
                            updateModel (userGroupOfModel.set <| Just userGroup)
                                >> updateModel (formOfModel.set <| Form.populateAddress userGroup model.form)

                        Err _ ->
                            identity
                   )

        AnswerQuestion question answer ->
            ( model, Cmd.none )
                |> updateModel (formOfModel.set <| Form.answerQuestion question.tag answer model.form)

        Submit ->
            ( model, Cmd.none )
                |> updateModel (formOfModel.set <| Form.validateAddress model.form)
