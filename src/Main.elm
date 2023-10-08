module Main exposing (main, viewInfoIcon)

-- import Data

import Browser
import Data exposing (userGroup)
import Html exposing (Html, text)
import Html.Attributes as Attr
import Html.Events exposing (onCheck, onClick, onMouseEnter, onMouseLeave)
import Http
import Icons
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
    { userGroup : Data
    , tabActive : TabsActive
    , showTooltip : Bool
    , toggleDropdown : Bool
    }


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
      , toggleDropdown = False
      }
    , fetchUserGroup
    )



---- UPDATE ----


type Msg
    = NoOp
    | GetUserGroup (Result Http.Error UserGroup)
    | ActiveTab TabsActive
    | ShowTooltip
    | AddPrefferedMethod PreferredContactMethod
    | ToggleDropdown


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

        AddPrefferedMethod method ->
            let
                updatedUserGroup =
                    case model.userGroup of
                        Success data ->
                            let
                                contactDetails =
                                    data.contactDetails

                                updatedContacts =
                                    { contactDetails | preferredContactMethod = method }

                                updatedGroup =
                                    { data | contactDetails = updatedContacts }
                            in
                            Success updatedGroup

                        _ ->
                            Loading
            in
            ( { model | toggleDropdown = False, userGroup = updatedUserGroup }, Cmd.none )

        ToggleDropdown ->
            ( { model | toggleDropdown = not <| model.toggleDropdown }, Cmd.none )


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

        "address" ->
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
                                        viewContactDetails userGroup.contactDetails model.toggleDropdown

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
                    , viewInputText { id = "preparation", value = settings.preparation, overrideClass = Just "mr-4 w-[45px]" }
                    , text <| pluralize settings.preparation "day"
                    ]
                ]
            , Html.fieldset [ Attr.class "mb-4" ]
                [ Html.label [ Attr.class "flex items-center", Attr.for "closed" ]
                    [ Html.span [ Attr.class "w-[94px]" ] [ text "Closed" ]
                    , viewInputText { id = "closed", value = settings.closed, overrideClass = Just "mr-4 w-[45px]" }
                    , text <| pluralize settings.closed "day"
                    ]
                ]
            , Html.fieldset [ Attr.class "mb-4" ]
                [ Html.label [ Attr.class "flex items-center", Attr.for "canceled" ]
                    [ Html.span [ Attr.class "w-[94px]" ] [ text "Canceled" ]
                    , viewInputText { id = "canceled", value = settings.canceled, overrideClass = Just "mr-4 w-[45px]" }
                    , text <| pluralize settings.canceled "day"
                    ]
                ]
            , Html.fieldset [ Attr.class "mb-4" ]
                [ Html.label [ Attr.class "flex items-center", Attr.for "timedOut" ]
                    [ Html.span [ Attr.class "w-[94px]" ] [ text "Timed out" ]
                    , viewInputText { id = "timedOut", value = settings.timedOut, overrideClass = Just "mr-4 w-[45px]" }
                    , text <| pluralize settings.timedOut "day"
                    ]
                ]
            , Html.fieldset [ Attr.class "mb-4" ]
                [ Html.label [ Attr.class "flex items-center", Attr.for "error" ]
                    [ Html.span [ Attr.class "w-[94px]" ] [ text "Error" ]
                    , viewInputText { id = "error", value = settings.error, overrideClass = Just "mr-4 w-[45px]" }
                    , text <| pluralize settings.error "day"
                    ]
                ]
            , Html.fieldset []
                [ Html.label [ Attr.class "flex items-center", Attr.for "shouldTrash" ]
                    [ Html.span [ Attr.class "flex items-center w-[94px]" ]
                        [ text "Destroy"
                        , viewInfoIcon "You confirm that item can be deleted after defined amount od days" showTooltip
                        ]
                    , Html.input [ Attr.class "ml-4 w-[20px] h-[20px] border-0 text-blue-400 shadow-sm ring-transparent focus:ring-0 focus:ring-offset-0 cursor-pointer border-blue-400,", Attr.type_ "checkbox", Attr.id "shouldTrash", Attr.checked settings.shouldTrash ] []
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


viewInputText : { id : String, value : String, overrideClass : Maybe String } -> Html Msg
viewInputText { id, value, overrideClass } =
    Html.input
        [ Attr.class "ml-4 rounded-sm border-0 py-1 px-2 h-10 text-gray-900 shadow-sm h-[30px] text-sm ring-transparent focus:ring-transparent"
        , Attr.class
            (case overrideClass of
                Just override ->
                    override

                Nothing ->
                    ""
            )
        , Attr.type_ "text"
        , Attr.id id
        , Attr.value value
        ]
        []


viewContactDetails : ContactDetails -> Bool -> Html Msg
viewContactDetails contactDetails shouldShowDropdown =
    Html.section []
        [ -- [ Html.h3 [ Attr.class "text-2xl" ] [ text "Details form" ]
          Html.p [ Attr.class "mb-10" ] [ text "User's contact details" ]
        , Html.form [ Attr.class "flex flex-col" ]
            [ if contactDetails.isInherited then
                Html.div
                    [ Attr.id "overlay"
                    , Attr.class "bg-gray-200 w-[100%] h-[100%]"
                    ]
                    []

              else
                text ""
            , Html.div [ Attr.class "flex mb-20 items-center" ]
                [ Html.div [] [ text "Choose preferred contact method" ]
                , viewPreferredMethodDropdown { prefMethod = contactDetails.preferredContactMethod, shouldShowDropdown = shouldShowDropdown }
                ]
            , Html.fieldset [ Attr.class "mb-4" ]
                [ Html.label
                    [ Attr.class "flex items-center", Attr.for "email" ]
                    [ Html.span [ Attr.class "w-[132px]" ] [ text "Email" ]
                    , Html.span [ Attr.class "flex flex-col" ]
                        [ if contactDetails.preferredContactMethod == Email then
                            Html.span [ Attr.class "ml-4 text-xs border-blue-400 border rounded-t text-blue-400 pl-2" ] [ text "required" ]

                          else
                            text ""
                        , viewInputText
                            { id = "email"
                            , value = contactDetails.email
                            , overrideClass =
                                if contactDetails.preferredContactMethod == Email then
                                    Just "rounded-t-none ring-blue-400 ring-1"

                                else
                                    Nothing
                            }
                        ]
                    ]
                ]
            , Html.fieldset [ Attr.class "mb-4" ]
                [ Html.label
                    [ Attr.class "flex items-center", Attr.for "phone" ]
                    [ Html.span [ Attr.class "w-[132px]" ] [ text "Phone" ]
                    , Html.span [ Attr.class "flex flex-col" ]
                        [ if contactDetails.preferredContactMethod == Phone then
                            Html.span [ Attr.class "ml-4 text-xs border-blue-400 border rounded-t text-blue-400 pl-2" ] [ text "required" ]

                          else
                            text ""
                        , viewInputText
                            { id = "phone"
                            , value = contactDetails.phone
                            , overrideClass =
                                if contactDetails.preferredContactMethod == Phone then
                                    Just "rounded-t-none ring-blue-400 ring-1"

                                else
                                    Nothing
                            }
                        ]
                    ]
                ]
            , Html.fieldset [ Attr.class "mb-4" ]
                [ Html.label
                    [ Attr.class "flex items-center", Attr.for "companyName" ]
                    [ Html.span [ Attr.class "w-[132px]" ] [ text "Company Name" ]
                    , viewInputText { id = "companyName", value = contactDetails.companyName, overrideClass = Nothing }
                    ]
                ]
            , Html.fieldset [ Attr.class "mb-4" ]
                [ Html.label
                    [ Attr.class "flex items-center", Attr.for "address" ]
                    [ Html.span [ Attr.class "w-[132px]" ] [ text "Address" ]
                    , Html.span [ Attr.class "flex flex-col" ]
                        [ if contactDetails.preferredContactMethod == Post then
                            Html.span [ Attr.class "ml-4 text-xs border-blue-400 border rounded-t text-blue-400 pl-2" ] [ text "required" ]

                          else
                            text ""
                        , viewInputText
                            { id = "address"
                            , value = contactDetails.address
                            , overrideClass =
                                if contactDetails.preferredContactMethod == Post then
                                    Just "rounded-t-none ring-blue-400 ring-1"

                                else
                                    Nothing
                            }
                        ]
                    ]
                ]
            , Html.fieldset [ Attr.class "mb-4" ]
                [ Html.label
                    [ Attr.class "flex items-center", Attr.for "zip" ]
                    [ Html.span [ Attr.class "w-[132px]" ] [ text "Zip" ]
                    , viewInputText { id = "zip", value = contactDetails.zip, overrideClass = Nothing }
                    ]
                ]
            , Html.fieldset [ Attr.class "mb-4" ]
                [ Html.label
                    [ Attr.class "flex items-center", Attr.for "city" ]
                    [ Html.span [ Attr.class "w-[132px]" ] [ text "City" ]
                    , viewInputText { id = "city", value = contactDetails.city, overrideClass = Nothing }
                    ]
                ]
            , Html.fieldset []
                [ Html.label
                    [ Attr.class "flex items-center", Attr.for "country" ]
                    [ Html.span [ Attr.class "w-[132px]" ] [ text "Country" ]
                    , viewInputText { id = "country", value = contactDetails.country, overrideClass = Nothing }
                    ]
                ]
            ]
        ]


viewPreferredMethodDropdown : { prefMethod : PreferredContactMethod, shouldShowDropdown : Bool } -> Html Msg
viewPreferredMethodDropdown { prefMethod, shouldShowDropdown } =
    Html.div [ Attr.class "ml-4 relative cursor-pointer" ]
        [ -- Html.div [] [ text <| preferredToString contactDetails.preferredContactMethod ]
          Html.div
            [ Attr.class "py-1 px-2 flex rounded items-center text-white bg-blue-400"
            , onClick ToggleDropdown
            ]
            [ text <| preferredToString prefMethod
            , Html.span
                [ Attr.class "w-[20px] ml-1"
                , if shouldShowDropdown then
                    Attr.class "rotate-180"

                  else
                    Attr.class ""
                ]
                [ Icons.arrowDown ]
            ]
        , Html.div
            [ Attr.class "absolute top-[30px] rounded-b bg-blue-400 overflow-hidden w-full text-white"
            , if shouldShowDropdown then
                Attr.class "block"

              else
                Attr.class "hidden"
            ]
            (case prefMethod of
                Email ->
                    [ Html.div [ Attr.class "py-1 px-2 hover:bg-blue-300 active:bg-blue-500", onClick (AddPrefferedMethod Phone) ]
                        [ text "Phone" ]
                    , Html.div
                        [ Attr.class "py-1 px-2 hover:bg-blue-300 active:bg-blue-500", onClick (AddPrefferedMethod Post) ]
                        [ text "Post" ]
                    ]

                Phone ->
                    [ Html.div [ Attr.class "py-1 px-2 hover:bg-blue-300 active:bg-blue-500", onClick (AddPrefferedMethod Email) ]
                        [ text "Email" ]
                    , Html.div
                        [ Attr.class "py-1 px-2 hover:bg-blue-300 active:bg-blue-500", onClick (AddPrefferedMethod Post) ]
                        [ text "Post" ]
                    ]

                Post ->
                    [ Html.div [ Attr.class "py-1 px-2 hover:bg-blue-300 active:bg-blue-500", onClick (AddPrefferedMethod Email) ]
                        [ text "Email" ]
                    , Html.div
                        [ Attr.class "py-1 px-2 hover:bg-blue-300 active:bg-blue-500", onClick (AddPrefferedMethod Phone) ]
                        [ text "Phone" ]
                    ]
            )
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
