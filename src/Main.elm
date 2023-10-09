module Main exposing (main, viewInfoIcon)

-- import Data

import Browser
import Data exposing (userGroup)
import Html exposing (Html, text, th)
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
                        ]
                    , Html.div [ Attr.class "" ]
                        [ Html.div [ Attr.class "flex flex-col" ]
                            [ Html.div []
                                [ viewTabs model.tabActive ]
                            , Html.div
                                [ Attr.class "bg-blue-100 p-4 mb-10 overflow-hidden" ]
                                [ case model.tabActive of
                                    SettingsTab ->
                                        viewSettings userGroup.settings model.showTooltip

                                    DetailsTab ->
                                        viewContactDetails userGroup.contactDetails model.toggleDropdown

                                    TagsTab ->
                                        viewTags userGroup.tags
                                ]
                            ]
                        , Html.button [ Attr.class "flex font-semi-bold px-5 py-2 rounded-full bg-blue-400 hover:bg-blue-300 active:bg-blue-500 easy-in-out transition-all text-white" ] [ Html.span [ Attr.class "mr-2 flex w-[20px]" ] [ Icons.checkIcon ], text "Submit" ]
                        ]
                    ]
        ]


viewTabs : TabsActive -> Html Msg
viewTabs tabActive =
    Html.ul [ Attr.class "flex text-3xl cursor-pointer" ]
        [ Html.li
            [ Attr.class "p-4"
            , Attr.class
                (if tabActive == SettingsTab then
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
                (if tabActive == DetailsTab then
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
                (if tabActive == TagsTab then
                    "bg-blue-100"

                 else
                    "bg-white"
                )
            , onClick (ActiveTab TagsTab)
            ]
            [ text "Tags" ]
        ]


viewTags : Tags -> Html Msg
viewTags tags =
    Html.section []
        [ Html.p [ Attr.class "mb-10" ] [ text "Add new tags, limit is 32 characters and they should be unique" ]
        , Html.form [ Attr.class "flex flex-col mb-10" ]
            [ Html.fieldset [ Attr.class "mb-4" ]
                [ Html.label [ Attr.class "flex items-center", Attr.for "newTag" ]
                    [ viewInputText { id = "newTag", value = "", overrideClass = Just "ml-0" }
                    , Html.button [ Attr.class "font-semi-bold flex items-center px-2 py-1 rounded-full bg-blue-400 rounded-l-none hover:bg-blue-300 active:bg-blue-500 transition-all text-white" ]
                        [ Html.span [ Attr.class "w-[20px] flex" ] [ Icons.plusIcon ]
                        , text "Add tag"
                        ]
                    ]
                ]
            ]
        , Html.ul [ Attr.class "flex gap-4 flex-wrap" ]
            (tags
                |> List.filter (\{ value } -> String.length value < 32 && String.length value > 0)
                |> List.map
                    (\{ value } ->
                        Html.li [ Attr.class "flex bg-blue-200 text-blue-400 easy-in-out border border-blue-400 rounded" ]
                            [ Html.span [ Attr.class "px-2 py-1" ] [ text value ]
                            , Html.span [ Attr.class "bg-red-400 cursor-pointer text-red-700 px-1 py-2 text-xs" ]
                                [ Html.span [ Attr.class "flex w-[14px]" ] [ Icons.deleteIcon ] ]
                            , Html.span [ Attr.class "bg-gray-300 cursor-pointer text-gray-600 px-1 py-2 text-xs rounded-r" ]
                                [ Html.span [ Attr.class "flex w-[14px]" ] [ Icons.editIcon ] ]
                            ]
                    )
            )
        ]


viewSettings : Settings -> Bool -> Html Msg
viewSettings settings showTooltip =
    Html.section [ Attr.class "relative" ]
        [ if settings.isInherited then
            viewDisablePageOverlay

          else
            text ""
        , Html.p [ Attr.class "mb-10" ] [ text "How many days do you want document to stay in a certain state ?" ]
        , formWrapper
            [ viewFieldset
                { id = "preparation"
                , daysString = Just settings.preparation
                , requiredField = False
                , textValue = "Preparation"
                , inputValue = settings.preparation
                , maxWidthFieldClass = "w-[94px]"
                , overrideClass = Just "mr-4 w-[45px]"
                }
            , viewFieldset
                { id = "closed"
                , daysString = Just settings.closed
                , requiredField = False
                , textValue = "Closed"
                , inputValue = settings.closed
                , maxWidthFieldClass = "w-[94px]"
                , overrideClass = Just "mr-4 w-[45px]"
                }
            , viewFieldset
                { id = "canceled"
                , daysString = Just settings.canceled
                , requiredField = False
                , textValue = "Canceled"
                , inputValue = settings.canceled
                , maxWidthFieldClass = "w-[94px]"
                , overrideClass = Just "mr-4 w-[45px]"
                }
            , viewFieldset
                { id = "timedOut"
                , daysString = Just settings.timedOut
                , requiredField = False
                , textValue = "Timed out"
                , inputValue = settings.timedOut
                , maxWidthFieldClass = "w-[94px]"
                , overrideClass = Just "mr-4 w-[45px]"
                }
            , viewFieldset
                { id = "error"
                , daysString = Just settings.error
                , requiredField = False
                , textValue = "Error"
                , inputValue = settings.error
                , maxWidthFieldClass = "w-[94px]"
                , overrideClass = Just "mr-4 w-[45px]"
                }
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


viewFieldset :
    { id : String
    , daysString : Maybe String
    , requiredField : Bool
    , textValue : String
    , inputValue : String
    , maxWidthFieldClass : String
    , overrideClass : Maybe String
    }
    -> Html Msg
viewFieldset { id, daysString, requiredField, textValue, inputValue, maxWidthFieldClass, overrideClass } =
    Html.fieldset [ Attr.class "mb-4" ]
        [ Html.label [ Attr.class "flex items-center", Attr.for id ]
            [ Html.span [ Attr.class maxWidthFieldClass ] [ text textValue ]
            , if requiredField then
                Html.span [ Attr.class "flex flex-col" ]
                    [ Html.span [ Attr.class "ml-4 text-xs border-blue-400 border rounded-t text-blue-400 pl-2" ] [ text "required" ]
                    , viewInputText { id = id, value = inputValue, overrideClass = overrideClass }
                    ]

              else
                viewInputText { id = id, value = inputValue, overrideClass = overrideClass }
            , case daysString of
                Just day ->
                    text <| pluralize day "day"

                Nothing ->
                    text ""
            ]
        ]


formWrapper : List (Html Msg) -> Html Msg
formWrapper children =
    Html.form [ Attr.class "flex flex-col" ]
        children


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
            Html.p [ Attr.class "absolute -top-1 left-[20px] w-[420px] rounded text-xs bg-black p-1 flex " ] [ text txt ]

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
    Html.section [ Attr.class "relative" ]
        [ if contactDetails.isInherited then
            viewDisablePageOverlay

          else
            text ""
        , Html.p [ Attr.class "mb-10" ] [ text "User's contact details" ]
        , formWrapper
            [ Html.div [ Attr.class "flex mb-10 items-center" ]
                [ Html.div [] [ text "Choose preferred contact method" ]
                , viewPreferredMethodDropdown { prefMethod = contactDetails.preferredContactMethod, shouldShowDropdown = shouldShowDropdown }
                ]
            , viewFieldset
                { id = "email"
                , daysString = Nothing
                , requiredField = contactDetails.preferredContactMethod == Email
                , textValue = "Email"
                , inputValue = contactDetails.email
                , maxWidthFieldClass = "w-[132px]"
                , overrideClass =
                    if contactDetails.preferredContactMethod == Email then
                        Just "rounded-t-none ring-blue-400 ring-1"

                    else
                        Nothing
                }
            , viewFieldset
                { id = "phone"
                , daysString = Nothing
                , requiredField = contactDetails.preferredContactMethod == Phone
                , textValue = "Phone"
                , inputValue = contactDetails.phone
                , maxWidthFieldClass = "w-[132px]"
                , overrideClass =
                    if contactDetails.preferredContactMethod == Phone then
                        Just "rounded-t-none ring-blue-400 ring-1"

                    else
                        Nothing
                }
            , viewFieldset
                { id = "companyName"
                , daysString = Nothing
                , requiredField = False
                , textValue = "Company Name"
                , inputValue = contactDetails.companyName
                , maxWidthFieldClass = "w-[132px]"
                , overrideClass = Nothing
                }
            , viewFieldset
                { id = "address"
                , daysString = Nothing
                , requiredField = contactDetails.preferredContactMethod == Post
                , textValue = "Address"
                , inputValue = contactDetails.address
                , maxWidthFieldClass = "w-[132px]"
                , overrideClass =
                    if contactDetails.preferredContactMethod == Post then
                        Just "rounded-t-none ring-blue-400 ring-1"

                    else
                        Nothing
                }
            , viewFieldset
                { id = "zip"
                , daysString = Nothing
                , requiredField = False
                , textValue = "Zip"
                , inputValue = contactDetails.zip
                , maxWidthFieldClass = "w-[132px]"
                , overrideClass = Nothing
                }
            , viewFieldset
                { id = "city"
                , daysString = Nothing
                , requiredField = False
                , textValue = "City"
                , inputValue = contactDetails.city
                , maxWidthFieldClass = "w-[132px]"
                , overrideClass = Nothing
                }
            , viewFieldset
                { id = "country"
                , daysString = Nothing
                , requiredField = False
                , textValue = "Country"
                , inputValue = contactDetails.country
                , maxWidthFieldClass = "w-[132px]"
                , overrideClass = Nothing
                }
            ]
        ]


viewDisablePageOverlay : Html msg
viewDisablePageOverlay =
    Html.div []
        [ Html.div [ Attr.class "z-12 text-red-400 mb-4" ]
            [ Html.div [ Attr.class "flex" ]
                [ Html.span [ Attr.class "w-[20px] flex mr-2" ] [ Icons.cautionIcon ]
                , Html.p [] [ text "It's not possible to change data on this page" ]
                ]
            ]
        , Html.div
            [ Attr.id "overlay"
            , Attr.class "bg-blue-100 opacity-60 w-[100%] h-[100%] absolute z-10"
            ]
            []
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
                    [ viewDropdownItem { textValue = "Phone", toMsg = AddPrefferedMethod Phone }
                    , viewDropdownItem { textValue = "Post", toMsg = AddPrefferedMethod Post }
                    ]

                Phone ->
                    [ viewDropdownItem { textValue = "Email", toMsg = AddPrefferedMethod Email }
                    , viewDropdownItem
                        { textValue = "Post", toMsg = AddPrefferedMethod Post }
                    ]

                Post ->
                    [ viewDropdownItem { textValue = "Email", toMsg = AddPrefferedMethod Email }
                    , viewDropdownItem { textValue = "Phone", toMsg = AddPrefferedMethod Phone }
                    ]
            )
        ]


viewDropdownItem : { textValue : String, toMsg : Msg } -> Html Msg
viewDropdownItem { textValue, toMsg } =
    Html.div [ Attr.class "py-1 px-2 hover:bg-blue-300 active:bg-blue-500", onClick toMsg ] [ text textValue ]



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
