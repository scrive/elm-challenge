module Form.Tag exposing
    ( FormType(..)
    , Model
    , Msg
    , ParentMsg(..)
    , Tag
    , decodeTag
    , initAddFormModel
    , initEditFormModel
    , update
    , view
    )

import Dict exposing (Dict)
import Html exposing (Html)
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline as Pipeline


type Model
    = Model ModelData


type alias ModelData =
    { errors : Errors
    , tag : Tag
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


type alias Tag =
    { name : String
    , value : String
    }


emptyErrors : Errors
emptyErrors =
    Dict.empty


emptyTag : Tag
emptyTag =
    { name = ""
    , value = ""
    }


initAddFormModel : Model
initAddFormModel =
    Model
        { errors = emptyErrors
        , tag = emptyTag
        , formType = Add
        }


initEditFormModel : Tag -> Model
initEditFormModel tag =
    Model
        { errors = emptyErrors
        , tag = tag
        , formType = Edit
        }


decodeTag : Decoder Tag
decodeTag =
    Decode.succeed Tag
        |> Pipeline.required "name" Decode.string
        |> Pipeline.optional "value" Decode.string ""


type Msg
    = InsertedInputValue InputId String
    | ClickedCancel
    | ClickedSubmit


type ParentMsg
    = NoUpdate
    | HideEditForm
    | SubmittedForm Tag


update : List Tag -> Msg -> Model -> ( Model, Cmd Msg, ParentMsg )
update tags msg ((Model ({ tag } as modelData)) as model) =
    case msg of
        InsertedInputValue Name name ->
            let
                updatedTag =
                    { tag | name = name }
            in
            ( Model { modelData | tag = updatedTag }
            , Cmd.none
            , NoUpdate
            )

        InsertedInputValue Value value ->
            let
                updatedTag =
                    { tag | value = value }
            in
            ( Model { modelData | tag = updatedTag }
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
    Html.text "Tags"
