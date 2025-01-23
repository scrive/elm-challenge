module Update exposing (update)

import Data
import Data.UserGroup as UserGroup
import Json.Decode as JD
import Lenses exposing (userGroupOfModel)
import Model exposing (Model)
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

                        Err _ ->
                            identity
                   )
