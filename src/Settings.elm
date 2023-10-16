module Settings exposing (State, Msg, init, update, view)

import Html exposing (Html)
import Html.Attributes
import Html.Events
import Form exposing (Form)
import Form.Field as Field
import Form.Input as Input
import Form.Validate as Validate exposing (..)
import Form.Error as Error exposing (..)
import UserGroup exposing (Settings, DataRetentionPolicy)
import Errors exposing (errorString)


type alias State =
    { form : Form () DataRetentionPolicy
    , inherited : Bool
    }


type Msg
    = FormMsg Form.Msg



---- INIT ----


init : Settings -> State
init settings =
    let
        fields =
            [ ( "idleDocTimeoutPreparation"
              , settings.dataRetentionPolicy.idleDocTimeoutPreparation
              |> Maybe.map String.fromInt
              |> Maybe.withDefault ""
              |> Field.string
              )
            , ( "idleDocTimeoutPreparationEnabled"
              , case settings.dataRetentionPolicy.idleDocTimeoutPreparation of
                    Just _ ->
                        Field.bool True

                    Nothing ->
                        Field.bool False
              )
            , ( "idleDocTimeoutClosed"
              , settings.dataRetentionPolicy.idleDocTimeoutClosed
              |> Maybe.map String.fromInt
              |> Maybe.withDefault ""
              |> Field.string
              )
            , ( "idleDocTimeoutClosedEnabled"
              , case settings.dataRetentionPolicy.idleDocTimeoutClosed of
                    Just _ ->
                        Field.bool True

                    Nothing ->
                        Field.bool False
              )
            , ( "idleDocTimeoutCanceled"
              , settings.dataRetentionPolicy.idleDocTimeoutCanceled
              |> Maybe.map String.fromInt
              |> Maybe.withDefault ""
              |> Field.string
              )
            , ( "idleDocTimeoutCanceledEnabled"
              , case settings.dataRetentionPolicy.idleDocTimeoutCanceled of
                    Just _ ->
                        Field.bool True

                    Nothing ->
                        Field.bool False
              )
            , ( "idleDocTimeoutTimedout"
              , settings.dataRetentionPolicy.idleDocTimeoutTimedout
              |> Maybe.map String.fromInt
              |> Maybe.withDefault ""
              |> Field.string
              )
            , ( "idleDocTimeoutTimedoutEnabled"
              , case settings.dataRetentionPolicy.idleDocTimeoutTimedout of
                    Just _ ->
                        Field.bool True

                    Nothing ->
                        Field.bool False
              )
            , ( "idleDocTimeoutRejected"
              , settings.dataRetentionPolicy.idleDocTimeoutRejected
              |> Maybe.map String.fromInt
              |> Maybe.withDefault ""
              |> Field.string
              )
            , ( "idleDocTimeoutRejectedEnabled"
              , case settings.dataRetentionPolicy.idleDocTimeoutRejected of
                    Just _ ->
                        Field.bool True

                    Nothing ->
                        Field.bool False
              )
            , ( "idleDocTimeoutError"
              , settings.dataRetentionPolicy.idleDocTimeoutError
              |> Maybe.map String.fromInt
              |> Maybe.withDefault ""
              |> Field.string
              )
            , ( "idleDocTimeoutErrorEnabled"
              , case settings.dataRetentionPolicy.idleDocTimeoutError of
                    Just _ ->
                        Field.bool True

                    Nothing ->
                        Field.bool False
              )
            , ( "immediateTrash", Field.bool settings.dataRetentionPolicy.immediateTrash )
            ]
    in
        { form = Form.initial fields validate
        , inherited = settings.inheritedFrom /= Nothing
        }



---- VALIDATION ----


validate : Validation () DataRetentionPolicy
validate =
    succeed DataRetentionPolicy
        |> andMap (timeout "idleDocTimeoutPreparation")
        |> andMap (timeout "idleDocTimeoutClosed")
        |> andMap (timeout "idleDocTimeoutCanceled")
        |> andMap (timeout "idleDocTimeoutTimedout")
        |> andMap (timeout "idleDocTimeoutRejected")
        |> andMap (timeout "idleDocTimeoutError")
        |> andMap (field "immediateTrash" bool)


timeout : String -> Validation () (Maybe Int)
timeout path =
    field (path ++ "Enabled") bool
        |> andThen (\enabled ->
                        if enabled then
                            field path (int |> andThen (minInt 1) |> map Just)
                        else
                            succeed Nothing
                   )



---- UPDATE ----


update : Msg -> State -> ( State, Cmd Msg )
update msg state =
    case msg of
        FormMsg formMsg ->
            let
                form = Form.update validate formMsg state.form
            in
                ( { state | form = form }
                , Cmd.none
                )



---- VIEW ----


view : State -> Html Msg
view state =
    Html.map FormMsg (formView state.form state.inherited)


formView : Form () DataRetentionPolicy -> Bool -> Html Form.Msg
formView form disabled =
    Html.form
        [ Html.Attributes.class "mt-10 grid grid-cols-1 gap-x-6 gap-y-4 sm:grid-cols-6 mx-auto max-w-2xl"
        , Html.Events.onSubmit Form.Submit
        ]
        [ Html.h2
              [ Html.Attributes.class "col-span-full text-base font-semibold leading-7 text-gray-900" ]
              [ Html.text "Data Retention Policy" ]
        , timeoutField "idleDocTimeoutPreparation" "Preparation" disabled form
        , timeoutField "idleDocTimeoutClosed" "Closed" disabled form
        , timeoutField "idleDocTimeoutCanceled" "Canceled" disabled form
        , timeoutField "idleDocTimeoutTimedout" "Timedout" disabled form
        , timeoutField "idleDocTimeoutRejected" "Rejected" disabled form
        , timeoutField "idleDocTimeoutError" "Error" disabled form
        , trashField "immediateTrash" "Immediate trash" disabled form
        , Html.button
              [ Html.Attributes.class "rounded-md bg-indigo-600 px-3 py-2 text-sm font-semibold text-white shadow-sm hover:bg-indigo-500 focus-visible:outline focus-visible:outline-2 focus-visible:outline-offset-2 focus-visible:outline-indigo-600 disabled:bg-slate-400 disabled:text-slate-50 disabled:border-slate-200 disabled:shadow-none"
              , Html.Attributes.disabled disabled
              ]
              [ Html.text "Save" ]
        ]


timeoutField : String -> String -> Bool -> Form () DataRetentionPolicy -> Html Form.Msg
timeoutField path label disabled form =
    let
        field =
            Form.getFieldAsString path form

        enabled =
            Form.getFieldAsBool (path ++ "Enabled") form
    in
        Html.div [ Html.Attributes.class "sm:col-span-full" ]
            [ Html.div [ Html.Attributes.class "relative flex gap-x-3 items-center"]
                  [ Input.checkboxInput enabled
                        [ Html.Attributes.id enabled.path
                        , Html.Attributes.class "h-4 w-4 rounded border-gray-300 text-indigo-600 focus:ring-indigo-600"
                        , Html.Attributes.disabled disabled
                        ]
                  , Html.label
                        [ Html.Attributes.for enabled.path
                        , case field.liveError of
                              Just _ ->
                                  Html.Attributes.class "block text-sm font-medium leading-6 text-red-700"

                              Nothing ->
                                  Html.Attributes.class "block text-sm font-medium leading-6 text-gray-900"
                        ]
                        [ Html.text label ]
                  ]
            , if enabled.value == Just True then
                  Html.div [ Html.Attributes.class "mt-2"]
                      [ Input.textInput field
                            [ Html.Attributes.id field.path
                            , Html.Attributes.class "block rounded-md border-0 px-2 py-1.5 text-gray-900 shadow-sm ring-1 ring-inset ring-gray-300 placeholder:text-gray-400 focus:ring-2 focus:ring-inset focus:ring-indigo-600 sm:text-sm sm:leading-6 disabled:bg-slate-50 disabled:text-slate-500 disabled:border-slate-200 disabled:shadow-none"
                            , case field.liveError of
                                  Just _ ->
                                      Html.Attributes.class "bg-red-50 border border-red-500 text-red-900 placeholder-red-700 text-sm rounded-lg focus:ring-red-500 focus:border-red-500 p-2.5 dark:bg-red-100 dark:border-red-400"
                          
                                  Nothing ->
                                      Html.Attributes.class ""
                            , Html.Attributes.disabled disabled
                            ]
                      ]
              else
                  Html.div [] []
            , errorFor field
            ]


trashField : String -> String -> Bool -> Form () DataRetentionPolicy -> Html Form.Msg
trashField path label disabled form =
    let
        field =
            Form.getFieldAsBool path form
    in
        Html.div [ Html.Attributes.class "sm:col-span-full" ]
            [ Html.div [ Html.Attributes.class "relative flex gap-x-3"]
                  [ Html.div [ Html.Attributes.class "flex h-6 items-center" ]
                        [ Input.checkboxInput field
                              [ Html.Attributes.id field.path
                              , Html.Attributes.class "h-4 w-4 rounded border-gray-300 text-indigo-600 focus:ring-indigo-600"
                              , Html.Attributes.disabled disabled
                              ]
                        ]
                  , Html.div [ Html.Attributes.class "text-sm leading-6" ]
                      [ Html.label
                            [ Html.Attributes.for field.path
                            , case field.liveError of
                                  Just _ ->
                                      Html.Attributes.class "block text-sm font-medium leading-6 text-red-700"

                                  Nothing ->
                                      Html.Attributes.class "block text-sm font-medium leading-6 text-gray-900"
                            ]
                            [ Html.text label ]
                      , Html.p [ Html.Attributes.class "text-gray-500" ]
                          [ Html.text "Immediatly trash the document." ]
                      ]
                  ]
            , errorFor field
            ]


errorFor : Form.FieldState () a -> Html msg
errorFor field =
    case field.liveError of
        Just error ->
            Html.div
                [ Html.Attributes.class "mt-2 text-sm text-red-600" ]
                [ Html.text (errorString error) ]
                        
        Nothing ->
            Html.text ""

