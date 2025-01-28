module Component.DataRetentionSettings exposing (..)

import Browser.Dom as Dom
import Data.UserGroup exposing (DataRetentionPolicy)
import Html exposing (..)
import Html.Attributes as Attrs exposing (..)
import Html.Events as Events
import Maybe.Extra as Maybe
import Task


type alias Model =
    { isInherited : Bool
    , idleDocTimeoutPreparation : DataRetentionField
    , idleDocTimeoutClosed : DataRetentionField
    , idleDocTimeoutCanceled : DataRetentionField
    , idleDocTimeoutTimedout : DataRetentionField
    , idleDocTimeoutRejected : DataRetentionField
    , idleDocTimeoutError : DataRetentionField
    , initialImmediateTrash : Bool
    , immediateTrash : Bool
    }


toPolicy : DataRetentionField -> Maybe Int
toPolicy field =
    if field.isChecked then
        field.currentValue
            |> String.toInt

    else
        Nothing


toDataRetentionPolicy : Model -> DataRetentionPolicy
toDataRetentionPolicy model =
    { idleDocTimeoutPreparation = toPolicy model.idleDocTimeoutPreparation
    , idleDocTimeoutClosed = toPolicy model.idleDocTimeoutClosed
    , idleDocTimeoutCanceled = toPolicy model.idleDocTimeoutCanceled
    , idleDocTimeoutTimedout = toPolicy model.idleDocTimeoutTimedout
    , idleDocTimeoutRejected = toPolicy model.idleDocTimeoutRejected
    , idleDocTimeoutError = toPolicy model.idleDocTimeoutError
    , immediateTrash = model.immediateTrash
    }


type alias DataRetentionField =
    { initialValue : Maybe Int
    , currentValue : String
    , isChecked : Bool
    }


init : Bool -> DataRetentionPolicy -> Model
init isInherited dataRetentionPolicy =
    let
        toDataRetentionField field =
            { initialValue = field
            , currentValue = Maybe.unwrap "" String.fromInt field
            , isChecked = Maybe.isJust field
            }
    in
    { isInherited = isInherited
    , idleDocTimeoutPreparation = toDataRetentionField dataRetentionPolicy.idleDocTimeoutPreparation
    , idleDocTimeoutClosed = toDataRetentionField dataRetentionPolicy.idleDocTimeoutClosed
    , idleDocTimeoutCanceled = toDataRetentionField dataRetentionPolicy.idleDocTimeoutCanceled
    , idleDocTimeoutTimedout = toDataRetentionField dataRetentionPolicy.idleDocTimeoutTimedout
    , idleDocTimeoutRejected = toDataRetentionField dataRetentionPolicy.idleDocTimeoutRejected
    , idleDocTimeoutError = toDataRetentionField dataRetentionPolicy.idleDocTimeoutError
    , initialImmediateTrash = dataRetentionPolicy.immediateTrash
    , immediateTrash = dataRetentionPolicy.immediateTrash
    }


updateIsChecked : Bool -> DataRetentionField -> DataRetentionField
updateIsChecked isChecked field =
    { field | isChecked = isChecked }


updateCurrentValue : String -> DataRetentionField -> DataRetentionField
updateCurrentValue currentValue field =
    { field | currentValue = currentValue }



-- Update --


type Msg
    = NoOp
    | IdleDocTimeoutPreparationChecked Bool
    | IdleDocTimeoutPreparationInputChanged String
    | IdleDocTimeoutClosedChecked Bool
    | IdleDocTimeoutClosedInputChanged String
    | IdleDocTimeoutCanceledChecked Bool
    | IdleDocTimeoutCanceledInputChanged String
    | IdleDocTimeoutTimedoutChecked Bool
    | IdleDocTimeoutTimedoutInputChanged String
    | IdleDocTimeoutRejectedChecked Bool
    | IdleDocTimeoutRejectedInputChanged String
    | IdleDocTimeoutErrorChecked Bool
    | IdleDocTimeoutErrorInputChanged String
    | ImmediateTrashChecked Bool
    | ApplyButtonClicked


focusIfChecked : String -> Bool -> Cmd Msg
focusIfChecked elId isChecked =
    if isChecked then
        Task.attempt (\_ -> NoOp) (Dom.focus elId)

    else
        Cmd.none


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        IdleDocTimeoutPreparationChecked isChecked ->
            ( { model | idleDocTimeoutPreparation = updateIsChecked isChecked model.idleDocTimeoutPreparation }
            , focusIfChecked "preparation-inp" isChecked
            )

        IdleDocTimeoutPreparationInputChanged input ->
            ( { model | idleDocTimeoutPreparation = updateCurrentValue input model.idleDocTimeoutPreparation }, Cmd.none )

        IdleDocTimeoutClosedChecked isChecked ->
            ( { model | idleDocTimeoutClosed = updateIsChecked isChecked model.idleDocTimeoutClosed }
            , focusIfChecked "closed-inp" isChecked
            )

        IdleDocTimeoutClosedInputChanged input ->
            ( { model | idleDocTimeoutClosed = updateCurrentValue input model.idleDocTimeoutClosed }, Cmd.none )

        IdleDocTimeoutCanceledChecked isChecked ->
            ( { model | idleDocTimeoutCanceled = updateIsChecked isChecked model.idleDocTimeoutCanceled }
            , focusIfChecked "canceled-inp" isChecked
            )

        IdleDocTimeoutCanceledInputChanged input ->
            ( { model | idleDocTimeoutCanceled = updateCurrentValue input model.idleDocTimeoutCanceled }, Cmd.none )

        IdleDocTimeoutTimedoutChecked isChecked ->
            ( { model | idleDocTimeoutTimedout = updateIsChecked isChecked model.idleDocTimeoutTimedout }
            , focusIfChecked "timedout-inp" isChecked
            )

        IdleDocTimeoutTimedoutInputChanged input ->
            ( { model | idleDocTimeoutTimedout = updateCurrentValue input model.idleDocTimeoutTimedout }, Cmd.none )

        IdleDocTimeoutRejectedChecked isChecked ->
            ( { model | idleDocTimeoutRejected = updateIsChecked isChecked model.idleDocTimeoutRejected }
            , focusIfChecked "rejected-inp" isChecked
            )

        IdleDocTimeoutRejectedInputChanged input ->
            ( { model | idleDocTimeoutRejected = updateCurrentValue input model.idleDocTimeoutRejected }, Cmd.none )

        IdleDocTimeoutErrorChecked isChecked ->
            ( { model | idleDocTimeoutError = updateIsChecked isChecked model.idleDocTimeoutError }
            , focusIfChecked "errors-inp" isChecked
            )

        IdleDocTimeoutErrorInputChanged input ->
            ( { model | idleDocTimeoutError = updateCurrentValue input model.idleDocTimeoutError }, Cmd.none )

        ImmediateTrashChecked isChecked ->
            ( { model | immediateTrash = isChecked }, Cmd.none )

        _ ->
            ( model, Cmd.none )



-- Validation --


isFieldValid : DataRetentionField -> Bool
isFieldValid field =
    if field.isChecked then
        String.toInt field.currentValue
            |> Maybe.isJust

    else
        True


isFormValid : Model -> Bool
isFormValid model =
    List.all isFieldValid
        [ model.idleDocTimeoutPreparation
        , model.idleDocTimeoutClosed
        , model.idleDocTimeoutCanceled
        , model.idleDocTimeoutTimedout
        , model.idleDocTimeoutRejected
        , model.idleDocTimeoutError
        ]


isFieldChanged : DataRetentionField -> Bool
isFieldChanged field =
    if field.isChecked then
        String.toInt field.currentValue /= field.initialValue

    else
        Maybe.isJust field.initialValue


isFormChanged : Model -> Bool
isFormChanged model =
    List.any isFieldChanged
        [ model.idleDocTimeoutPreparation
        , model.idleDocTimeoutClosed
        , model.idleDocTimeoutCanceled
        , model.idleDocTimeoutTimedout
        , model.idleDocTimeoutRejected
        , model.idleDocTimeoutError
        ]
        || (model.initialImmediateTrash /= model.immediateTrash)



-- View --


checkBoxField : String -> Bool -> (Bool -> Msg) -> (String -> Msg) -> String -> DataRetentionField -> Html Msg
checkBoxField id_ isInherited toCheckedMsg toInputMsg labelText field =
    div [ class "flex items-center mb-4" ]
        [ input
            [ checked field.isChecked
            , Events.onCheck <| toCheckedMsg
            , disabled isInherited
            , id <| id_ ++ "-cbx"
            , type_ "checkbox"
            , value ""
            , class "w-4 h-4 text-blue-600 bg-gray-100 border-gray-300 rounded focus:ring-blue-500 focus:ring-2"
            ]
            []
        , span [ class "ms-2 text-sm font-medium" ]
            [ text labelText
            , input
                [ type_ "number"
                , id <| id_ ++ "-inp"
                , value field.currentValue
                , disabled <| not field.isChecked || isInherited
                , Events.onInput <| toInputMsg
                , Attrs.min "1"
                , classList [ ( "border-red-500", Maybe.isNothing <| String.toInt field.currentValue ) ]
                , class "disabled:bg-slate-50 disabled:text-slate-500 disabled:border-slate-200 disabled:shadow-none w-20 appearance-none border rounded py-2 px-3 text-gray-700 leading-tight focus:outline-none focus:shadow-outline"
                ]
                [ text field.currentValue ]
            , text " days"
            ]
        ]


settingsForm : Model -> Html Msg
settingsForm settings =
    fieldset []
        [ legend [ class "sr-only" ] [ text "Data retention policies" ]
        , checkBoxField "preparation" settings.isInherited IdleDocTimeoutPreparationChecked IdleDocTimeoutPreparationInputChanged "Move documents in preparation to trash in " settings.idleDocTimeoutPreparation
        , checkBoxField "closed" settings.isInherited IdleDocTimeoutClosedChecked IdleDocTimeoutClosedInputChanged "Move closed documents to trash in " settings.idleDocTimeoutClosed
        , checkBoxField "canceled" settings.isInherited IdleDocTimeoutCanceledChecked IdleDocTimeoutCanceledInputChanged "Move canceled documents to trash in " settings.idleDocTimeoutCanceled
        , checkBoxField "timedout" settings.isInherited IdleDocTimeoutTimedoutChecked IdleDocTimeoutTimedoutInputChanged "Move timed out documents to trash in " settings.idleDocTimeoutTimedout
        , checkBoxField "rejected" settings.isInherited IdleDocTimeoutRejectedChecked IdleDocTimeoutRejectedInputChanged "Move rejected documents to trash in " settings.idleDocTimeoutRejected
        , checkBoxField "errors" settings.isInherited IdleDocTimeoutErrorChecked IdleDocTimeoutErrorInputChanged "Move documents with errors to trash in " settings.idleDocTimeoutError
        , hr [] []
        , div [ class "flex items-center mt-4" ]
            [ input
                [ checked settings.immediateTrash
                , Events.onCheck ImmediateTrashChecked
                , id "trash-cbx"
                , type_ "checkbox"
                , disabled settings.isInherited
                , value ""
                , class "w-4 h-4 text-blue-600 bg-gray-100 border-gray-300 rounded focus:ring-blue-500 focus:ring-2"
                ]
                []
            , label [ for "trash-cbx", class "ms-2 text-sm font-medium" ]
                [ span [] [ text "Delete documents in trash immediately" ] ]
            ]
        ]


applyButton : Model -> Html Msg
applyButton model =
    if model.isInherited then
        text ""

    else
        let
            isButtonDisabled =
                (not <| isFormValid model) || (not <| isFormChanged model)
        in
        button
            [ class "mt-8 bg-blue-500 text-white font-bold py-2 px-4 rounded"
            , classList [ ( "opacity-50 cursor-not-allowed", isButtonDisabled ) ]
            , disabled <| isButtonDisabled
            , Events.onClick ApplyButtonClicked
            ]
            [ text "Apply" ]


introductionText : String
introductionText =
    """Settings in this page include the data retention policies.
    Select a document state and fill in the number of days to set how long it takes for the document in that state to
    move to trash or de-select a policy to remove it from the active policies. Select trash immediately to delete 
    the documents that land in the trash immediately. """


inheritedText : Bool -> List (Html Msg)
inheritedText isInherited =
    if isInherited then
        [ text "The current settings are "
        , span [ class "font-bold" ] [ text "inherited" ]
        , text " from a parent user group, meaning that they can not be modified."
        ]

    else
        []


view : Model -> Html Msg
view model =
    div []
        [ div [ class "mb-8 text-sm" ] <|
            text introductionText
                :: inheritedText model.isInherited
        , settingsForm model
        , applyButton model
        ]
