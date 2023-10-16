module ContactDetails exposing (State, Msg, init, update, view)

import Html exposing (Html)
import Html.Attributes
import Html.Events
import Form exposing (Form)
import Form.Field as Field
import Form.Input as Input
import Form.Validate as Validate exposing (..)
import Form.Error as Error exposing (..)
import Regex
import UserGroup exposing (ContactDetails, Address)
import Errors exposing (errorString)


type alias State =
    { form : Form () Address
    , inherited : Bool
    }


type Msg
    = FormMsg Form.Msg



---- INIT ----


init : ContactDetails -> State
init contactDetails =
    let
        fields =
            [ ( "preferredContactMethod"
              , Field.string contactDetails.address.preferredContactMethod
              )
            , ( "email"
              , contactDetails.address.email
              |> Maybe.withDefault ""
              |> Field.string
              )
            , ( "phone"
              , contactDetails.address.phone
              |> Maybe.withDefault ""
              |> Field.string
              )
            , ( "companyName"
              , contactDetails.address.companyName
              |> Maybe.withDefault ""
              |> Field.string
              )
            , ( "address"
              , contactDetails.address.address
              |> Maybe.withDefault ""
              |> Field.string
              )
            , ( "zip"
              , contactDetails.address.zip
              |> Maybe.withDefault ""
              |> Field.string
              )
            , ( "city"
              , contactDetails.address.city
              |> Maybe.withDefault ""
              |> Field.string
              )
            , ( "country"
              , contactDetails.address.country
              |> Maybe.withDefault ""
              |> Field.string
              )
            ]
    in
        { form = Form.initial fields validate
        , inherited = contactDetails.inheritedFrom /= Nothing
        }



---- VALIDATION ----
            

validate : Validation () Address
validate =
    let
        address contactMethod =
            case contactMethod of
                "email" ->
                    succeed Address
                        |> andMap (succeed contactMethod)
                        |> andMap (field "email" (required email))
                        |> andMap (field "phone" (optional phone))
                        |> andMap (field "companyName" (optional string))
                        |> andMap (field "address" (optional string))
                        |> andMap (field "zip" (optional string))
                        |> andMap (field "city" (optional string))
                        |> andMap (field "country" (optional string))
                
                "phone" ->
                    succeed Address
                        |> andMap (succeed contactMethod)
                        |> andMap (field "email" (optional email))
                        |> andMap (field "phone" (required phone))
                        |> andMap (field "companyName" (optional string))
                        |> andMap (field "address" (optional string))
                        |> andMap (field "zip" (optional string))
                        |> andMap (field "city" (optional string))
                        |> andMap (field "country" (optional string))
                
                "post" ->
                    succeed Address
                        |> andMap (succeed contactMethod)
                        |> andMap (field "email" (optional email))
                        |> andMap (field "phone" (optional phone))
                        |> andMap (field "companyName" (required string))
                        |> andMap (field "address" (required string))
                        |> andMap (field "zip" (required string))
                        |> andMap (field "city" (required string))
                        |> andMap (field "country" (required string))

                _ ->
                    fail (value NotIncludedIn)
    in
        (field "preferredContactMethod" string) |> andThen address


required : Validation () String -> Validation () (Maybe String)
required validation =
    validation |> andThen nonEmpty |> map Just


optional : Validation () String -> Validation () (Maybe String)
optional validation =
    oneOf [ emptyString |> andThen (\_ -> succeed Nothing)
          , validation |> andThen nonEmpty |> map Just
          ]


phone : Validation () String
phone =
    let
        regex =
            Maybe.withDefault Regex.never <|
                (Regex.fromString "^[+]*[(]{0,1}[0-9]{1,4}[)]{0,1}[-\\s\\./0-9]*$")
    in
        string |> andThen (format regex)



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


formView : Form () Address -> Bool -> Html Form.Msg
formView form disabled =
    Html.form
        [ Html.Attributes.class "mt-10 grid grid-cols-1 gap-x-6 gap-y-8 sm:grid-cols-6 mx-auto max-w-2xl"
        , Html.Events.onSubmit Form.Submit
        ]
        [ Html.h2
              [ Html.Attributes.class "col-span-full text-base font-semibold leading-7 text-gray-900" ]
              [ Html.text "Contact details" ]
        , selectField "preferredContactMethod" "Preferred contact method"
              [ ( "email", "Email" )
              , ( "phone", "Phone" )
              , ( "post", "Post" )
              ]
              disabled
              form
        , textField "email" "Email" disabled form
        , textField "phone" "Phone" disabled form
        , textField "companyName" "Company name" disabled form
        , textField "address" "Address" disabled form
        , textField "zip" "Zip" disabled form
        , textField "city" "City" disabled form
        , textField "country" "Country" disabled form
        , Html.button
              [ Html.Attributes.class "rounded-md bg-indigo-600 px-3 py-2 text-sm font-semibold text-white shadow-sm hover:bg-indigo-500 focus-visible:outline focus-visible:outline-2 focus-visible:outline-offset-2 focus-visible:outline-indigo-600 disabled:bg-slate-400 disabled:text-slate-50 disabled:border-slate-200 disabled:shadow-none"
              , Html.Attributes.disabled disabled
              ]
              [ Html.text "Save" ]
        ]
        
        
selectField : String -> String -> List ( String, String ) -> Bool -> Form () Address -> Html Form.Msg
selectField path label options disabled form =
    let
        field =
            Form.getFieldAsString path form
    in
        Html.div [ Html.Attributes.class "sm:col-span-full"]
            [ Html.label
                  [ Html.Attributes.for field.path
                  , Html.Attributes.class "block text-sm font-medium leading-6 text-gray-900" ]
                [ Html.text label ]
            , Html.div [ Html.Attributes.class "mt-2"]
                  [ Input.selectInput options field
                      [ Html.Attributes.id field.path
                      , Html.Attributes.class "block w-full rounded-md border-0 py-1.5 text-gray-900 shadow-sm ring-1 ring-inset ring-gray-300 focus:ring-2 focus:ring-inset focus:ring-indigo-600 sm:max-w-xs sm:text-sm sm:leading-6 disabled:bg-slate-50 disabled:text-slate-500 disabled:border-slate-200 disabled:shadow-none"
                      , Html.Attributes.disabled disabled
                      ]
                ]
            , errorFor field
            ]


textField : String -> String -> Bool -> Form () Address -> Html Form.Msg
textField path label disabled form =
    let
        field =
            Form.getFieldAsString path form
    in
        Html.div [ Html.Attributes.class "sm:col-span-full"]
            [ Html.label
                  [ Html.Attributes.for field.path
                  , case field.liveError of
                        Just _ ->
                            Html.Attributes.class "block text-sm font-medium leading-6 text-red-700"

                        Nothing ->
                            Html.Attributes.class "block text-sm font-medium leading-6 text-gray-900"
                  ]
                [ Html.text label ]
            , Html.div [ Html.Attributes.class "mt-2"]
                [ Input.textInput field
                      [ Html.Attributes.id field.path
                      , Html.Attributes.class "block w-full rounded-md border-0 px-2 py-1.5 text-gray-900 shadow-sm ring-1 ring-inset ring-gray-300 placeholder:text-gray-400 focus:ring-2 focus:ring-inset focus:ring-indigo-600 sm:text-sm sm:leading-6 disabled:bg-slate-50 disabled:text-slate-500 disabled:border-slate-200 disabled:shadow-none"
                      , case field.liveError of
                            Just _ ->
                                Html.Attributes.class "bg-red-50 border border-red-500 text-red-900 placeholder-red-700 text-sm rounded-lg focus:ring-red-500 focus:border-red-500 block w-full p-2.5 dark:bg-red-100 dark:border-red-400"
                          
                            Nothing ->
                                Html.Attributes.class ""
                      , Html.Attributes.disabled disabled
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
