module Tags exposing (State, Msg, init, update, view)

import Dict
import Html exposing (Html)
import Html.Attributes
import Html.Events
import Form exposing (Form)
import Form.Field as Field
import Form.Input as Input
import Form.Validate as Validate exposing (..)
import Form.Error as Error exposing (..)
import UserGroup exposing (Tags)
import Errors exposing (errorString)


type alias State =
    { form : Form () Tags }


type Msg
    = FormMsg Form.Msg



---- INIT ----


init : Tags -> State
init tags =
    let
        fields =
            [ ( "tags", Field.list (List.map (Field.group << tagFields) (Dict.toList tags)) ) ]

        tagFields ( name, value ) =
            [ ( "name", Field.string name )
            , ( "value", value |> Maybe.withDefault "" |> Field.string )
            ]
    in
        { form = Form.initial fields validate }



---- VALIDATION ----


validate : Validation () Tags
validate =
    field "tags" (list tag)
        |> andThen (\t -> succeed (Dict.fromList t))


tag : Validation () (String, Maybe String)
tag =
    map2 Tuple.pair
        (field "name" (string |> andThen (maxLength 32)))
        (field "value" (oneOf [ string |> map Just
                              , emptyString |> andThen (\_ -> succeed Nothing)
                              ]))



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
    Html.map FormMsg (formView state.form)


formView : Form () Tags -> Html Form.Msg
formView form =
    Html.form
        [ Html.Attributes.class "mt-10 grid grid-cols-1 gap-x-6 gap-y-4 sm:grid-cols-6 mx-auto max-w-2xl"
        , Html.Events.onSubmit Form.Submit
        ]
        [ Html.h2
              [ Html.Attributes.class "col-span-full text-base font-semibold leading-7 text-gray-900" ]
              [ Html.text "Tags" ]
        , Html.div [ Html.Attributes.class "col-span-full" ] <|
            List.map (tagView "tags" form)
                (Form.getListIndexes "tags" form)
        , Html.div [ Html.Attributes.class "col-span-full" ]
            [ Html.a
                  [ Html.Events.onClick (Form.Append "tags")
                  , Html.Attributes.class "text-sm font-semibold text-indigo-600"
                  , Html.Attributes.href "#"
                  ]
                  [ Html.text "+ Add new tag" ]
            ]
        , Html.button
              [ Html.Attributes.class "rounded-md bg-indigo-600 px-3 py-2 text-sm font-semibold text-white shadow-sm hover:bg-indigo-500 focus-visible:outline focus-visible:outline-2 focus-visible:outline-offset-2 focus-visible:outline-indigo-600 disabled:bg-slate-400 disabled:text-slate-50 disabled:border-slate-200 disabled:shadow-none"
              ]
              [ Html.text "Save" ]
        ]


tagView : String -> Form () Tags -> Int -> Html Form.Msg
tagView path form idx =
    let
        prefix =
            path ++ "." ++ (String.fromInt idx)
            
        nameField =
            Form.getFieldAsString (prefix ++ ".name") form

        valueField =
            Form.getFieldAsString (prefix ++ ".value") form
    in
        Html.div [ Html.Attributes.class "sm:col-span-full" ]
            [ Html.div [ Html.Attributes.class "relative flex gap-x-3 items-top"]
                  [ textField (prefix ++ ".name") "Name" form
                  , textField (prefix ++ ".value") "Value" form
                  , Html.div [ Html.Attributes.class "py-3" ]
                      [ Html.a
                            [ Html.Events.onClick (Form.RemoveItem path idx)
                            , Html.Attributes.class "text-sm font-semibold text-indigo-600"
                            , Html.Attributes.href "#"
                            ]
                            [ Html.text "Remove" ]
                      ]
                  ]
            ]


textField : String -> String -> Form () Tags -> Html Form.Msg
textField path label form =
    let
        field =
            Form.getFieldAsString path form
    in
        Html.div [ Html.Attributes.class "sm:col-span-full"]
            [ Html.div [ Html.Attributes.class "mt-2"]
                [ Input.textInput field
                      [ Html.Attributes.id field.path
                      , Html.Attributes.placeholder label
                      , Html.Attributes.class "block w-full rounded-md border-0 px-2 py-1.5 text-gray-900 shadow-sm ring-1 ring-inset ring-gray-300 placeholder:text-gray-400 focus:ring-2 focus:ring-inset focus:ring-indigo-600 sm:text-sm sm:leading-6 disabled:bg-slate-50 disabled:text-slate-500 disabled:border-slate-200 disabled:shadow-none"
                      , case field.liveError of
                            Just _ ->
                                Html.Attributes.class "bg-red-50 border border-red-500 text-red-900 placeholder-red-700 text-sm rounded-lg focus:ring-red-500 focus:border-red-500 block w-full p-2.5 dark:bg-red-100 dark:border-red-400"
                          
                            Nothing ->
                                Html.Attributes.class ""
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

