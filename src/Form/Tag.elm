module Form.Tag exposing
    ( FormType(..)
    , Model
    , Msg
    , ParentMsg(..)
    , initAddFormModel
    , initEditFormModel
    , update
    , view
    )

import Data.Tag as Tag
import Dict exposing (Dict)
import Html exposing (Html)


type Model
    = Model ModelData


type alias ModelData =
    { errors : Errors
    , tag : Tag.Model
    , formType : FormType
    }


type alias Errors =
    Dict InputId (List String)


type FormType
    = Add
    | Edit


type InputId
    = Name
    | Value


emptyErrors : Errors
emptyErrors =
    Dict.empty


initAddFormModel : Model
initAddFormModel =
    Model
        { errors = emptyErrors
        , tag = Tag.emptyTag
        , formType = Add
        }


initEditFormModel : Tag.Model -> Model
initEditFormModel tag =
    Model
        { errors = emptyErrors
        , tag = tag
        , formType = Edit
        }


type Msg
    = InsertedInputValue InputId String
    | ClickedCancel
    | ClickedSubmit


type ParentMsg
    = NoUpdate
    | HideEditForm
    | SubmittedForm Tag.Model


update : Msg -> Model -> ( Model, Cmd Msg, ParentMsg )
update msg ((Model ({ tag } as modelData)) as model) =
    case msg of
        InsertedInputValue Name name ->
            ( Model { modelData | tag = Tag.updateName name tag }
            , Cmd.none
            , NoUpdate
            )

        InsertedInputValue Value value ->
            ( Model { modelData | tag = Tag.updateValue value tag }
            , Cmd.none
            , NoUpdate
            )

        ClickedCancel ->
            ( model
            , Cmd.none
            , HideEditForm
            )

        ClickedSubmit ->
            ( model
              -- clear form if validation passed
            , Cmd.none
            , SubmittedForm tag
            )


view : Model -> Html Msg
view (Model model) =
    Html.text "Tags form"
