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
import Html.Attributes as Attributes
import Ui.Button as Button
import Ui.Form as Form
import Ui.Input as Input
import Util.Validate as Validate


type Model
    = Model ModelData


type alias ModelData =
    { errors : Errors
    , tag : Tag.Model
    , originalTag : Maybe Tag.Model
    }


type alias Errors =
    Dict String Validate.Errors


type FormType
    = Add
    | Edit


type InputId
    = Name
    | Value


inputIdToString : InputId -> String
inputIdToString inputId =
    case inputId of
        Name ->
            "name"

        Value ->
            "value"


emptyErrors : Errors
emptyErrors =
    Dict.empty


initAddFormModel : Model
initAddFormModel =
    Model
        { errors = emptyErrors
        , tag = Tag.emptyTag
        , originalTag = Nothing
        }


initEditFormModel : Tag.Model -> Model
initEditFormModel tag =
    Model
        { errors = emptyErrors
        , tag = tag
        , originalTag = Just tag
        }


type Msg
    = InsertedInputValue InputId String
    | ClickedSubmit


type ParentMsg
    = NoUpdate
    | SubmittedForm Tag.Model


update : Tag.Tags -> Msg -> Model -> ( Model, Cmd Msg, ParentMsg )
update tags msg (Model ({ tag, originalTag } as modelData)) =
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

        ClickedSubmit ->
            let
                skipDuplicityValidation =
                    originalTag
                        |> Maybe.map (Tag.isSameTagName tag)
                        |> Maybe.withDefault False

                errors =
                    validate skipDuplicityValidation tag tags
            in
            if Dict.isEmpty errors then
                ( initAddFormModel
                , Cmd.none
                , SubmittedForm tag
                )

            else
                ( Model { modelData | errors = errors }
                , Cmd.none
                , NoUpdate
                )


validate : Bool -> Tag.Model -> Tag.Tags -> Errors
validate skipDuplicityValidation tag tags =
    [ { inputId = Name
      , validateInput = Validate.validateTagName skipDuplicityValidation
      }
    , { inputId = Value
      , validateInput = Validate.validateTagValue
      }
    ]
        |> List.foldl
            (\{ inputId, validateInput } errors ->
                let
                    errorsToInput =
                        validateInput tag tags

                    inputKey =
                        inputIdToString inputId
                in
                if List.length errorsToInput == 0 then
                    errors

                else
                    Dict.insert inputKey errorsToInput errors
            )
            Dict.empty


view : Model -> Html Msg
view (Model { errors, tag }) =
    Form.view ClickedSubmit
        [ Input.viewWithLabel []
            { msg = InsertedInputValue
            , placeholder = "Insert name"
            , value = Tag.getName tag
            , errors = inputErrorsWithDefault Name errors
            , inputId = Name
            , inputIdToString = inputIdToString
            , labelText = "Name"
            }
        , Input.viewWithLabel [ Attributes.class "mt-5" ]
            { msg = InsertedInputValue
            , placeholder = "Insert Value"
            , value = Tag.getValue tag
            , errors = inputErrorsWithDefault Value errors
            , inputId = Value
            , inputIdToString = inputIdToString
            , labelText = "Value"
            }
        , Html.div [ Attributes.class "flex justify-center mt-5" ]
            [ Button.submitButtonView []
                { text = "Submit"
                , size = Button.medium
                }
            ]
        ]


inputErrorsWithDefault : InputId -> Errors -> Validate.Errors
inputErrorsWithDefault inputId =
    Dict.get (inputIdToString inputId)
        >> Maybe.withDefault []
