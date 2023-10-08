module Main exposing (main, viewInfoIcon)

-- import Data

import Browser
import Css
import Data exposing (userGroup)
import Html exposing (Html, text)
import Html.Attributes as Attr
import Html.Events exposing (onCheck, onClick, onMouseEnter, onMouseLeave)
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


type TabsActive
    = SettingsTab
    | DetailsTab
    | TagsTab


type alias Model =
    { userGroup : Data, tabActive : TabsActive, showTooltip : Bool }


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
    ( { userGroup = Loading
      , tabActive = SettingsTab
      , showTooltip = False
      }
    , fetchUserGroup
    )



---- UPDATE ----


type Msg
    = NoOp
    | GetUserGroup (Result Http.Error UserGroup)
    | ActiveTab TabsActive
    | ShowTooltip


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

        ActiveTab tab ->
            ( { model | tabActive = tab }, Cmd.none )

        ShowTooltip ->
            ( { model | showTooltip = not <| model.showTooltip }, Cmd.none )


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
    Html.div [ Attr.class "flex flex-col w-[1024px] items-center mx-auto" ]
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
                Html.div [ Attr.class "flex flex-col mt-16 mb-48" ]
                    [ Html.div []
                        [ Html.div [ Attr.class "flex w-[100px] h-[21px] md:w-[140px] md:h-[30px]" ]
                            [ Html.img [ Attr.src "logo.png" ] []
                            ]
                        , Html.h1 [ Attr.class "text-5xl mb-10" ] [ text "User Group Settings Form" ]

                        -- , Html.p [] [ text "Here are 3 forms presented" ]
                        ]
                    , Html.div [ Attr.class "" ]
                        [ Html.div [ Attr.class "flex flex-col" ]
                            [ Html.div []
                                [ Html.ul [ Attr.class "flex text-3xl cursor-pointer" ]
                                    [ Html.li
                                        [ Attr.class "p-4"
                                        , Attr.class
                                            (if model.tabActive == SettingsTab then
                                                "bg-blue-100"

                                             else
                                                "bg-white"
                                            )
                                        , onClick (ActiveTab SettingsTab)
                                        ]
                                        [ text "Settings" ]
                                    , Html.li
                                        [ Attr.class "p-4"
                                        , Attr.class
                                            (if model.tabActive == DetailsTab then
                                                "bg-blue-100"

                                             else
                                                "bg-white"
                                            )
                                        , onClick (ActiveTab DetailsTab)
                                        ]
                                        [ text "Details" ]
                                    , Html.li
                                        [ Attr.class "p-4"
                                        , Attr.class
                                            (if model.tabActive == TagsTab then
                                                "bg-blue-100"

                                             else
                                                "bg-white"
                                            )
                                        , onClick (ActiveTab TagsTab)
                                        ]
                                        [ text "Tags" ]
                                    ]
                                ]
                            , Html.div
                                [ Attr.class "bg-blue-100 p-4 mb-10" ]
                                [ case model.tabActive of
                                    SettingsTab ->
                                        viewSettings userGroup.settings model.showTooltip

                                    DetailsTab ->
                                        viewContactDetails userGroup.contactDetails

                                    TagsTab ->
                                        viewTags userGroup.tags
                                ]
                            ]
                        , Html.button [ Attr.class "font-semi-bold px-5 py-2 rounded-full bg-blue-400 hover:bg-blue-300 active:bg-blue-500 transition-all text-white" ] [ text "Submit" ]
                        ]
                    ]
        ]


viewTags : Tags -> Html Msg
viewTags tags =
    Html.section []
        [ Html.h3 [ Attr.class "text-2xl" ] [ text "Tags form" ]
        , Html.p [] [ text "[placeholder for Tags description]" ]
        , Html.form [ Attr.class "flex flex-col" ]
            [ Html.label [ Attr.for "newTag" ]
                [ text "+ Add new tag"
                , Html.input [ Attr.class "ml-4", Attr.id "newTag", Attr.value "" ] []
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


viewSettings : Settings -> Bool -> Html Msg
viewSettings settings showTooltip =
    Html.section []
        [ -- Html.h3 [ Attr.class "text-2xl" ] [ text "Document Retention" ]
          Html.p [ Attr.class "mb-10" ] [ text "How many days do you want document to stay in a certain state ?" ]
        , Html.form [ Attr.class "flex flex-col" ]
            [ if settings.isInherited then
                Html.div
                    [ Attr.id "overlay"
                    , Attr.class "bg-gray-200 w-[100%] h-[100%]"
                    ]
                    []

              else
                text ""
            , Html.fieldset [ Attr.class "mb-4" ]
                [ Html.label [ Attr.class "flex items-center", Attr.for "preparation" ]
                    [ Html.span [ Attr.class "w-[94px]" ] [ text "Preparation" ]
                    , viewInputText { id = "preparation", value = settings.preparation, overrideClass = "mr-4 w-[45px]" }
                    , text <| pluralize settings.preparation "day"
                    ]
                ]
            , Html.fieldset [ Attr.class "mb-4" ]
                [ Html.label [ Attr.class "flex items-center", Attr.for "closed" ]
                    [ Html.span [ Attr.class "w-[94px]" ] [ text "Closed" ]
                    , viewInputText { id = "closed", value = settings.closed, overrideClass = "mr-4 w-[45px]" }
                    , text <| pluralize settings.closed "day"
                    ]
                ]
            , Html.fieldset [ Attr.class "mb-4" ]
                [ Html.label [ Attr.class "flex items-center", Attr.for "canceled" ]
                    [ Html.span [ Attr.class "w-[94px]" ] [ text "Canceled" ]
                    , viewInputText { id = "canceled", value = settings.canceled, overrideClass = "mr-4 w-[45px]" }
                    , text <| pluralize settings.canceled "day"
                    ]
                ]
            , Html.fieldset [ Attr.class "mb-4" ]
                [ Html.label [ Attr.class "flex items-center", Attr.for "timedOut" ]
                    [ Html.span [ Attr.class "w-[94px]" ] [ text "Timed out" ]
                    , viewInputText { id = "timedOut", value = settings.timedOut, overrideClass = "mr-4 w-[45px]" }
                    , text <| pluralize settings.timedOut "day"
                    ]
                ]
            , Html.fieldset [ Attr.class "mb-4" ]
                [ Html.label [ Attr.class "flex items-center", Attr.for "error" ]
                    [ Html.span [ Attr.class "w-[94px]" ] [ text "Error" ]
                    , viewInputText { id = "error", value = settings.error, overrideClass = "mr-4 w-[45px]" }
                    , text <| pluralize settings.error "day"
                    ]
                ]
            , Html.fieldset [ Attr.class "mb-4" ]
                [ Html.label [ Attr.class "flex items-center", Attr.for "shouldTrash" ]
                    [ Html.span [ Attr.class "flex items-center w-[94px]" ]
                        [ text "Destroy"
                        , viewInfoIcon "You confirm that item can be deleted after defined amount od days" showTooltip
                        ]
                    , Html.input [ Attr.class "ml-4 w-[20px] h-[20px] text-blue-300 focus:ring-0 focus:ring-offset-0 cursor-pointer border-solid, border-blue-300,", Attr.type_ "checkbox", Attr.id "shouldTrash", Attr.checked settings.shouldTrash ] []
                    ]
                ]
            ]
        ]


pluralize : String -> String -> String
pluralize int str =
    if (int |> String.toInt |> Maybe.withDefault 0) == 1 then
        str

    else
        str ++ "s"


viewInfoIcon : String -> Bool -> Html Msg
viewInfoIcon txt showTooltip =
    Html.div [ Attr.class "ml-1 w-[17px] h-[17px] text-center text-sm text-white rounded-full bg-blue-400 cursor-pointer relative", onMouseEnter ShowTooltip, onMouseLeave ShowTooltip ]
        [ text "?"
        , if showTooltip then
            Html.p [ Attr.class "absolute w-[420px] rounded text-xs bg-black p-1 flex " ] [ text txt ]

          else
            text ""
        ]



-- [ Tw.block
--     , Tw.w_full
--     , Tw.form_input
--     , Tw.rounded_md
--     , Tw.border_0
--     , Tw.py_1_dot_5
--     , Tw.h_10
--     , Tw.text_color Tw.gray_900
--     , Tw.shadow_sm
--     , Tw.ring_1
--     , Tw.ring_inset
--     , Tw.placeholder_color Tw.gray_300
--     , Tw.text_lg
--     , Tw.ring_color Tw.gray_300
--     , Css.focus
--         [ Tw.ring_2
--         , Tw.ring_inset
--         , Tw.ring_color Tw.teal_400
--         ]
--     ]


viewInputText : { id : String, value : String, overrideClass : String } -> Html Msg
viewInputText { id, value, overrideClass } =
    Html.input
        [ Attr.class "ml-4 rounded-sm  border-0 py-1 px-2 h-10 text-gray-900 shadow-sm h-[30px] text-sm ring-transparent focus:ring-transparent"
        , Attr.class overrideClass
        , Attr.type_ "text"
        , Attr.id id
        , Attr.value value
        ]
        []


viewContactDetails : ContactDetails -> Html Msg
viewContactDetails contactDetails =
    Html.section []
        [ Html.h3 [ Attr.class "text-2xl" ] [ text "Details form" ]
        , Html.p [] [ text "[placeholder for details description]" ]
        , Html.form [ Attr.class "flex flex-col" ]
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
                [ Attr.class "mb-4", Attr.for "email" ]
                [ text "Email"
                , Html.input [ Attr.class "ml-4", Attr.type_ "text", Attr.id "email", Attr.value contactDetails.email ] []
                , if contactDetails.preferredContactMethod == Email then
                    Html.span [ Attr.id "tooltip" ] [ text "Email is mandatory field" ]

                  else
                    text ""
                ]
            , Html.label
                [ Attr.class "mb-4", Attr.for "phone" ]
                [ text "Phone"
                , Html.input [ Attr.type_ "text", Attr.id "phone", Attr.value contactDetails.phone ] []
                , if contactDetails.preferredContactMethod == Phone then
                    Html.span [ Attr.class "ml-4", Attr.id "tooltip" ] [ text "Phone is mandatory field !" ]

                  else
                    text ""
                ]
            , Html.label
                [ Attr.class "mb-4", Attr.for "companyName" ]
                [ text "Company Name"
                , Html.input [ Attr.class "ml-4", Attr.type_ "text", Attr.id "companyName", Attr.value contactDetails.companyName ] []
                ]
            , Html.label
                [ Attr.class "mb-4", Attr.for "address" ]
                [ text "Address"
                , Html.input [ Attr.class "ml-4", Attr.class "ml-4", Attr.type_ "text", Attr.id "address", Attr.value contactDetails.address ] []
                , if contactDetails.preferredContactMethod == Post then
                    Html.span [ Attr.id "tooltip" ] [ text "Post is mandatory field" ]

                  else
                    text ""
                ]
            , Html.label
                [ Attr.class "mb-4", Attr.for "zip" ]
                [ text "Zip"
                , Html.input [ Attr.class "ml-4", Attr.type_ "text", Attr.id "zip", Attr.value contactDetails.zip ] []
                ]
            , Html.label
                [ Attr.class "mb-4", Attr.for "city" ]
                [ text "City"
                , Html.input [ Attr.class "ml-4", Attr.type_ "text", Attr.id "city", Attr.value contactDetails.city ] []
                ]
            , Html.label
                [ Attr.class "mb-4", Attr.for "country" ]
                [ text "Country"
                , Html.input [ Attr.class "ml-4", Attr.type_ "text", Attr.id "country", Attr.value contactDetails.country ] []
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
