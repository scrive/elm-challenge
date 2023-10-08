module Main exposing (main)

-- import Data

import Browser
import Data exposing (userGroup)
import Html exposing (Html, text)
import Html.Attributes as Attr
import Http
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline exposing (optional, optionalAt, required, requiredAt)
import Util


serverUrl : String
serverUrl =
    "http://localhost:5010/restAPI"



---- MODEL ----


type Data
    = Initial
    | Loading
    | Error String
    | Success UserGroup


type alias Model =
    { userGroup : Data }


type alias UserGroup =
    { settings : Settings
    , contactDetails : ContactDetails
    , tags : Tags
    }



-- type User a
--     = RootUser
--     | ChildUser (User UserGroup)


type PreferredContactMethod
    = Email
    | Post
    | Phone


type alias Settings =
    { isInherited : Bool
    , preparation : String
    , closed : String
    , canceled : String
    , timedOut : String
    , rejected : String
    , error : String
    , shouldTrash : Bool
    }


type alias ContactDetails =
    { isInherited : Bool
    , preferredContactMethod : PreferredContactMethod
    , email : String
    , phone : String
    , companyName : String
    , address : String
    , zip : String
    , city : String
    , country : String
    }


type alias Tags =
    List Tag


type alias Tag =
    { name : String, value : String }


init : ( Model, Cmd Msg )
init =
    ( { userGroup = Loading }
    , fetchUserGroup
    )



---- UPDATE ----


type Msg
    = NoOp
    | GetUserGroup (Result Http.Error UserGroup)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        GetUserGroup (Err err) ->
            let
                error =
                    Util.buildErrorMessage err
            in
            ( { model | userGroup = Error error }, Cmd.none )

        GetUserGroup (Ok userGroup) ->
            ( { model | userGroup = Success userGroup }, Cmd.none )


fetchUserGroup : Cmd Msg
fetchUserGroup =
    Http.get
        { url = serverUrl
        , expect = Http.expectJson GetUserGroup decodeUserGroup
        }


decodeUserGroup : Decoder UserGroup
decodeUserGroup =
    Decode.succeed UserGroup
        |> required "settings" decodeSettings
        |> required "contact_details" decodeContactDetails
        |> required "tags" decodeTags


decodeSettings : Decoder Settings
decodeSettings =
    Decode.succeed Settings
        |> required "inherited_from" decodeInheritFrom
        |> optionalAt [ "data_retention_policy", "idle_doc_timeout_preparation" ] (Decode.int |> Decode.map String.fromInt) ""
        |> optionalAt [ "data_retention_policy", "idle_doc_timeout_closed" ] (Decode.int |> Decode.map String.fromInt) ""
        |> optionalAt [ "data_retention_policy", "idle_doc_timeout_canceled" ] (Decode.int |> Decode.map String.fromInt) ""
        |> optionalAt [ "data_retention_policy", "idle_doc_timeout_timedout" ] (Decode.int |> Decode.map String.fromInt) ""
        |> optionalAt [ "data_retention_policy", "idle_doc_timeout_rejected" ] (Decode.int |> Decode.map String.fromInt) ""
        |> optionalAt [ "data_retention_policy", "idle_doc_timeout_error" ] (Decode.int |> Decode.map String.fromInt) ""
        |> requiredAt [ "data_retention_policy", "immediate_trash" ] Decode.bool


decodeContactDetails : Decoder ContactDetails
decodeContactDetails =
    Decode.succeed ContactDetails
        |> required "inherited_from" decodeInheritFrom
        |> requiredAt [ "address", "preferred_contact_method" ] decodePrefMethod
        |> requiredAt [ "address", "email" ] Decode.string
        |> optionalAt [ "address", "phone" ] Decode.string ""
        |> requiredAt [ "address", "company_name" ] Decode.string
        |> requiredAt [ "address", "address" ] Decode.string
        |> requiredAt [ "address", "zip" ] Decode.string
        |> requiredAt [ "address", "city" ] Decode.string
        |> requiredAt [ "address", "country" ] Decode.string


decodeInheritFrom : Decoder Bool
decodeInheritFrom =
    Decode.maybe Decode.string |> Decode.andThen stringToBool


stringToBool : Maybe String -> Decoder Bool
stringToBool maybeStr =
    case maybeStr of
        Just _ ->
            Decode.succeed True

        Nothing ->
            Decode.succeed False


decodePrefMethod : Decoder PreferredContactMethod
decodePrefMethod =
    Decode.string |> Decode.andThen stringToPrefMethod


stringToPrefMethod : String -> Decoder PreferredContactMethod
stringToPrefMethod str =
    case str of
        "email" ->
            Decode.succeed Email

        "post" ->
            Decode.succeed Post

        "phone" ->
            Decode.succeed Phone

        _ ->
            Decode.fail <| "Invalid preferred method: " ++ str


decodeTags : Decoder (List Tag)
decodeTags =
    Decode.list decodeTag


decodeTag : Decoder Tag
decodeTag =
    Decode.succeed Tag
        |> required "name" Decode.string
        |> optional "value" Decode.string ""


preferredToString : PreferredContactMethod -> String
preferredToString prf =
    case prf of
        Email ->
            "Email"

        Phone ->
            "Phone"

        Post ->
            "Post"



---- VIEW ----


header : String -> Html msg
header text =
    Html.span [ Attr.class "p-2 text-5xl font-extrabold text-transparent bg-clip-text bg-gradient-to-br from-slate-400 to-slate-800" ]
        [ Html.text text ]


subheader : String -> Html msg
subheader text =
    Html.span [ Attr.class "p-2 text-2xl font-extrabold text-slate-800" ]
        [ Html.text text ]


view : Model -> Html Msg
view model =
    -- Html.div [ Attr.class "flex flex-col w-[1024px] items-center mx-auto mt-16 mb-48" ]
    --     [ header "Let's start your task"
    --     , subheader "Here are your data:"
    --     , Html.pre [ Attr.class "my-8 py-4 px-12 text-sm bg-slate-100 font-mono shadow rounded" ] [ Html.text Data.userGroup ]
    --     , header "Now turn them into form."
    --     , subheader "See README for details of the task. Good luck 🍀 "
    --     ]
    Html.div [ Attr.class "flex flex-col w-[1024px] items-center mx-auto mt-16 mb-48" ]
        [ case model.userGroup of
            Initial ->
                text ""

            Loading ->
                Html.div []
                    [ Html.span [ Attr.class "loader" ] []
                    ]

            Error error ->
                Html.div [ Attr.class "bg-red-500" ] [ text error ]

            Success userGroup ->
                Html.div []
                    [ Html.div []
                        [ Html.h1 [ Attr.class "" ] [ text "User Group Settings Form" ]
                        , Html.p [] [ text "Here are 3 forms presented" ]
                        ]

                    -- , Html.pre [ Attr.class "my-8 py-4 px-12 text-sm bg-slate-100 font-mono shadow rounded" ] [ Html.text <| Debug.toString userGroup ]
                    , Html.div []
                        [ Html.div []
                            [ Html.ul []
                                [ Html.li [] [ text "Settings" ]
                                , Html.li [] [ text "Contact Details" ]
                                , Html.li [] [ text "Tags" ]
                                ]
                            ]
                        , Html.div
                            []
                            [ viewSettings userGroup.settings
                            , viewContactDetails userGroup.contactDetails
                            , viewTags userGroup.tags
                            ]
                        ]
                    , Html.button [] [ text "Submit" ]
                    ]
        ]


viewTags : Tags -> Html Msg
viewTags tags =
    Html.section []
        [ Html.h3 [] [ text "Tags form" ]
        , Html.p [] [ text "[placeholder for Tags description]" ]
        , Html.form []
            [ Html.label [ Attr.for "newTag" ]
                [ text "+ Add new tag"
                , Html.input [ Attr.id "newTag", Attr.value "" ] []
                ]
            ]
        , Html.ul []
            (tags
                |> List.map
                    (\{ value } ->
                        Html.div []
                            [ Html.span [] [ text value ]
                            , Html.span [] [ text "delete" ]
                            , Html.span [] [ text "edit" ]
                            ]
                    )
            )
        ]


viewSettings : Settings -> Html Msg
viewSettings settings =
    Html.section []
        [ Html.h3 [] [ text "Settings form" ]
        , Html.p [] [ text "[placeholder for Settings description]" ]
        , Html.form []
            [ if settings.isInherited then
                Html.div
                    [ Attr.id "overlay"
                    , Attr.class "bg-gray-200 w-[100%] h-[100%]"
                    ]
                    []

              else
                text ""
            , Html.label [ Attr.for "preparation" ]
                [ text "Preparation"
                , Html.input [ Attr.type_ "text", Attr.id "preparation", Attr.value settings.preparation ] []
                ]
            , Html.label [ Attr.for "closed" ]
                [ text "Closed"
                , Html.input [ Attr.type_ "text", Attr.id "closed", Attr.value settings.closed ] []
                ]
            , Html.label [ Attr.for "canceled" ]
                [ text "Canceled"
                , Html.input [ Attr.class "input", Attr.id "canceled", Attr.type_ "text", Attr.value settings.canceled ] []
                ]
            , Html.label [ Attr.for "timedOut" ]
                [ text "Timed out"
                , Html.input [ Attr.type_ "text", Attr.id "timedOut", Attr.value settings.timedOut ] []
                ]
            , Html.label [ Attr.for "error" ]
                [ text "Error"
                , Html.input [ Attr.type_ "text", Attr.id "error", Attr.value settings.error ] []
                ]
            , Html.label [ Attr.for "shouldTrash" ]
                [ text "Should Trash ?"
                , Html.input [ Attr.type_ "checkbox", Attr.id "shouldTrash", Attr.checked settings.shouldTrash ] []
                ]
            ]
        ]


viewContactDetails : ContactDetails -> Html Msg
viewContactDetails contactDetails =
    Html.section []
        [ Html.h3 [] [ text "Contact Details form" ]
        , Html.p [] [ text "[placeholder for Contact details description]" ]
        , Html.form []
            [ if contactDetails.isInherited then
                Html.div
                    [ Attr.id "overlay"
                    , Attr.class "bg-gray-200 w-[100%] h-[100%]"
                    ]
                    []

              else
                text ""
            , Html.div []
                [ Html.div [] [ text "Choose preferred contact method" ]
                , Html.div []
                    [ Html.div [] [ text <| preferredToString contactDetails.preferredContactMethod ]
                    , Html.div []
                        [ Html.div [] [ text "Email" ]
                        , Html.div [] [ text "Phone" ]
                        , Html.div [] [ text "Post" ]
                        ]
                    ]
                ]
            , Html.label
                [ Attr.for "email" ]
                [ text "Email"
                , Html.input [ Attr.type_ "text", Attr.id "email", Attr.value contactDetails.email ] []
                , if contactDetails.preferredContactMethod == Email then
                    Html.span [ Attr.id "tooltip" ] [ text "Email is mandatory field" ]

                  else
                    text ""
                ]
            , Html.label
                [ Attr.for "phone" ]
                [ text "Phone"
                , Html.input [ Attr.type_ "text", Attr.id "phone", Attr.value contactDetails.phone ] []
                , if contactDetails.preferredContactMethod == Phone then
                    Html.span [ Attr.id "tooltip" ] [ text "Phone is mandatory field !" ]

                  else
                    text ""
                ]
            , Html.label
                [ Attr.for "companyName" ]
                [ text "Company Name"
                , Html.input [ Attr.type_ "text", Attr.id "companyName", Attr.value contactDetails.companyName ] []
                ]
            , Html.label
                [ Attr.for "address" ]
                [ text "Address"
                , Html.input [ Attr.type_ "text", Attr.id "address", Attr.value contactDetails.address ] []
                , if contactDetails.preferredContactMethod == Post then
                    Html.span [ Attr.id "tooltip" ] [ text "Post is mandatory field" ]

                  else
                    text ""
                ]
            , Html.label
                [ Attr.for "zip" ]
                [ text "Zip"
                , Html.input [ Attr.type_ "text", Attr.id "zip", Attr.value contactDetails.zip ] []
                ]
            , Html.label
                [ Attr.for "city" ]
                [ text "City"
                , Html.input [ Attr.type_ "text", Attr.id "city", Attr.value contactDetails.city ] []
                ]
            , Html.label
                [ Attr.for "country" ]
                [ text "Country"
                , Html.input [ Attr.type_ "text", Attr.id "country", Attr.value contactDetails.country ] []
                ]
            ]
        ]



---- PROGRAM ----


main : Program () Model Msg
main =
    Browser.application
        { view =
            \model ->
                { title = "Scrive elm challenge task"
                , body = [ view model ]
                }
        , init = \_ _ _ -> init
        , update = update
        , subscriptions = always Sub.none
        , onUrlRequest = always NoOp
        , onUrlChange = always NoOp
        }
