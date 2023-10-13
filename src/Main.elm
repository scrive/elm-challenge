module Main exposing (main, viewInfoIcon)

-- import Data

import Browser
import Data exposing (userGroup)
import Dict exposing (Dict)
import Html exposing (Html, text)
import Html.Attributes as Attr
import Html.Events exposing (onCheck, onClick, onInput, onMouseEnter, onMouseLeave)
import Http
import Icons
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline exposing (optional, optionalAt, required, requiredAt)
import Process
import Set exposing (Set)
import Task
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


type Tabs
    = SettingsTab
    | DetailsTab
    | TagsTab


type alias Model =
    { userGroup : Data
    , tabActive : Tabs
    , showTooltip : Bool
    , toggleDropdown : Bool
    , errors : Set String
    , formData : FormData
    , isFormSent : Bool
    }


type alias FormData =
    { inputFields : Dict String String
    , shouldTrash : Bool
    , tags : Tags
    , prefMethod : PreferredContactMethod
    }


type alias UserGroup =
    { settings : Settings
    , contactDetails : ContactDetails
    , tags : Tags
    }


type Ids
    = PrepId
    | CloseId
    | CancelId
    | TimeOutId
    | RejectId
    | ErrId
    | EmailId
    | PhoneId
    | CompanyId
    | PostId
    | ZipId
    | CityId
    | NewTagNameId
    | NewTagValId
    | CountryId


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


type alias TagName =
    String


type alias Tags =
    Dict TagName String


init : ( Model, Cmd Msg )
init =
    ( { userGroup = Loading
      , tabActive = SettingsTab
      , showTooltip = False
      , toggleDropdown = False
      , errors = Set.empty
      , formData = { inputFields = Dict.empty, shouldTrash = False, tags = Dict.empty, prefMethod = Email }
      , isFormSent = False
      }
    , fetchUserGroup
    )



---- UPDATE ----


type Msg
    = NoOp
    | GetUserGroup (Result Http.Error UserGroup)
    | ActiveTab Tabs
    | ShowTooltip
    | AddPrefferedMethod PreferredContactMethod
    | ToggleDropdown
    | OnInputChange Ids String
    | AddNewTag
    | DeleteTag TagName
    | EditTag TagName
    | ToggleCheckbox Bool
    | SubmitForm { isSettingsInherited : Bool, isDetailsInherited : Bool }
    | FormSent


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
            let
                { settings, contactDetails, tags } =
                    userGroup

                { shouldTrash, preparation, closed, canceled, timedOut, rejected, error } =
                    settings

                { email, phone, companyName, address, zip, city, country, preferredContactMethod } =
                    contactDetails

                { formData } =
                    model

                fillInputFields =
                    Dict.fromList [ ( "preparation", preparation ), ( "closed", closed ), ( "canceled", canceled ), ( "timedOut", timedOut ), ( "rejected", rejected ), ( "error", error ), ( "email", email ), ( "phone", phone ), ( "companyName", companyName ), ( "address", address ), ( "zip", zip ), ( "city", city ), ( "country", country ) ]

                updateFormData =
                    { formData
                        | inputFields = fillInputFields
                        , shouldTrash = shouldTrash
                        , tags = tags
                        , prefMethod = preferredContactMethod
                    }
            in
            ( { model | userGroup = Success userGroup, formData = updateFormData }, Cmd.none )

        ActiveTab tab ->
            ( { model | tabActive = tab }, Cmd.none )

        ShowTooltip ->
            ( { model | showTooltip = not <| model.showTooltip }, Cmd.none )

        AddPrefferedMethod method ->
            let
                { formData } =
                    model

                updateFormData =
                    { formData | prefMethod = method }
            in
            ( { model | errors = Set.empty, toggleDropdown = False, formData = updateFormData }, Cmd.none )

        ToggleCheckbox isChecked ->
            let
                { formData } =
                    model

                updateFormData =
                    { formData | shouldTrash = isChecked }
            in
            ( { model | formData = updateFormData }, Cmd.none )

        SubmitForm { isSettingsInherited, isDetailsInherited } ->
            let
                { formData } =
                    model

                { inputFields, prefMethod } =
                    formData

                updateErrors =
                    formValidate
                        { isSettingsInherited = isSettingsInherited
                        , isDetailsInherited = isDetailsInherited
                        , inputFields = inputFields
                        , prefMethod = prefMethod
                        }
            in
            ( { model | errors = updateErrors }
              -- We simulate that form is sent to a server
            , if Set.isEmpty updateErrors then
                Process.sleep 1000
                    |> Task.perform (\_ -> FormSent)

              else
                Cmd.none
            )

        FormSent ->
            ( { model | isFormSent = True }, Cmd.none )

        OnInputChange ids inputValue ->
            let
                { formData } =
                    model

                updateInputFields id input =
                    Dict.insert (fromIdToString id) input model.formData.inputFields

                updateFormData =
                    { formData | inputFields = updateInputFields ids inputValue }
            in
            ( { model | formData = updateFormData, isFormSent = False }, Cmd.none )

        ToggleDropdown ->
            ( { model | toggleDropdown = not <| model.toggleDropdown }, Cmd.none )

        AddNewTag ->
            let
                { formData } =
                    model

                { inputFields, tags } =
                    formData

                resetTagsInputs =
                    inputFields |> Dict.insert "newTagName" "" |> Dict.insert "newTagValue" ""

                currentTagName =
                    inputFields |> Dict.get "newTagName" |> Maybe.withDefault ""

                currentTagValue =
                    inputFields |> Dict.get "newTagValue" |> Maybe.withDefault ""
            in
            if String.length currentTagName > 32 then
                ( { model | errors = Set.insert "Tags Tab: Tag name should be less then 33 characters" model.errors }, Cmd.none )

            else if String.isEmpty currentTagName then
                ( { model | errors = Set.insert "Tags Tab: Tag name is empty" model.errors }, Cmd.none )

            else if String.isEmpty currentTagValue then
                let
                    updatedTags =
                        tags |> Dict.insert currentTagName ""

                    updatedFormData =
                        { formData
                            | tags = updatedTags
                            , inputFields = resetTagsInputs
                        }
                in
                ( { model | errors = Set.empty, formData = updatedFormData }, Cmd.none )

            else
                let
                    updatedTags =
                        tags |> Dict.insert currentTagName currentTagValue

                    updatedFormData =
                        { formData
                            | tags = updatedTags
                            , inputFields = resetTagsInputs
                        }
                in
                ( { model | errors = Set.empty, formData = updatedFormData }, Cmd.none )

        EditTag tagName ->
            let
                { formData } =
                    model

                { tags, inputFields } =
                    formData
            in
            case Dict.get tagName tags of
                Just tagValue ->
                    let
                        updateInputFields =
                            inputFields
                                |> Dict.insert "newTagName" tagName
                                |> Dict.insert "newTagValue" tagValue

                        updatedFormData =
                            { formData | inputFields = updateInputFields }
                    in
                    ( { model | formData = updatedFormData }, Cmd.none )

                Nothing ->
                    ( model, Cmd.none )

        DeleteTag tagName ->
            let
                { formData } =
                    model

                { tags } =
                    formData

                updatedTags =
                    tags |> Dict.remove tagName

                updatedFormData =
                    { formData | tags = updatedTags }
            in
            ( { model | formData = updatedFormData }, Cmd.none )


formValidate :
    { inputFields : Dict String String
    , prefMethod : PreferredContactMethod
    , isDetailsInherited : Bool
    , isSettingsInherited : Bool
    }
    -> Set String
formValidate { inputFields, prefMethod, isDetailsInherited, isSettingsInherited } =
    let
        emailValue =
            inputFields |> Dict.get "email" |> Maybe.withDefault ""

        phoneValue =
            inputFields |> Dict.get "phone" |> Maybe.withDefault ""

        settingsList =
            [ "preparation", "closed", "canceled", "timedOut", "rejected", "error" ]

        updateErrors =
            let
                detailsErrors =
                    -- We'll validate details only if not inherited
                    if isDetailsInherited then
                        Set.empty

                    else
                        let
                            emailError =
                                let
                                    errorMsg =
                                        "Details Tab: Invalid email"
                                in
                                case Util.parseEmail (inputFields |> Dict.get "email" |> Maybe.withDefault "") of
                                    Ok _ ->
                                        Set.remove errorMsg Set.empty

                                    Err _ ->
                                        Set.insert errorMsg Set.empty

                            phoneErrorMsg =
                                "Details Tab: Valid phone should contain '+' sign followed by 11 min digits"

                            phoneError =
                                case Util.parsePhoneNumber (inputFields |> Dict.get "phone" |> Maybe.withDefault "") of
                                    Ok _ ->
                                        Set.remove phoneErrorMsg Set.empty

                                    Err _ ->
                                        Set.insert phoneErrorMsg Set.empty

                            madatoryFieldsErrors =
                                case prefMethod of
                                    Email ->
                                        let
                                            errorMsg =
                                                "Details Tab: Email is mandatory field"
                                        in
                                        if String.isEmpty emailValue then
                                            Set.insert errorMsg Set.empty

                                        else
                                            Set.remove errorMsg Set.empty

                                    Post ->
                                        let
                                            addressValue =
                                                inputFields |> Dict.get "address" |> Maybe.withDefault ""

                                            addressErrorMsg =
                                                "Details Tab: Address is mandatory field"
                                        in
                                        if String.isEmpty addressValue then
                                            Set.insert addressErrorMsg Set.empty

                                        else
                                            Set.remove addressErrorMsg Set.empty

                                    Phone ->
                                        let
                                            errorMsg =
                                                "Details Tab: Phone is mandatory field"
                                        in
                                        if String.isEmpty phoneValue then
                                            Set.insert errorMsg Set.empty

                                        else
                                            Set.remove errorMsg Set.empty
                        in
                        [ emailError, phoneError, madatoryFieldsErrors ] |> List.foldl (\set acc -> Set.union set acc) Set.empty

                settingsErrors =
                    -- We'll validate settings only if not inherited
                    if isSettingsInherited then
                        Set.empty

                    else
                        inputFields
                            |> Dict.foldl
                                (\key value acc ->
                                    if List.member key settingsList then
                                        if String.toInt value == Nothing && String.length value /= 0 then
                                            Set.insert "Settings Tab: Field should be number" acc

                                        else
                                            acc

                                    else
                                        acc
                                )
                                Set.empty
            in
            Set.union detailsErrors settingsErrors
    in
    updateErrors


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


decodeTagTuple : Decoder ( TagName, String )
decodeTagTuple =
    Decode.succeed (\n v -> ( n, v ))
        |> required "name" Decode.string
        |> optional "value" Decode.string ""


decodeTags : Decoder (Dict TagName String)
decodeTags =
    Decode.list decodeTagTuple
        |> Decode.map Dict.fromList


preferredToString : PreferredContactMethod -> String
preferredToString prf =
    case prf of
        Email ->
            "Email"

        Phone ->
            "Phone"

        Post ->
            "Post"


fromIdToString : Ids -> String
fromIdToString ids =
    case ids of
        PrepId ->
            "preparation"

        CloseId ->
            "closed"

        CancelId ->
            "canceled"

        TimeOutId ->
            "timedOut"

        RejectId ->
            "rejected"

        ErrId ->
            "error"

        EmailId ->
            "email"

        PhoneId ->
            "phone"

        CompanyId ->
            "companyName"

        PostId ->
            "address"

        ZipId ->
            "zip"

        CityId ->
            "city"

        NewTagNameId ->
            "newTagName"

        NewTagValId ->
            "newTagValue"

        CountryId ->
            "countryName"



---- VIEW ----


view : Model -> Html Msg
view model =
    Html.div [ Attr.class "flex flex-col sm:w-full md:w-[626px] items-center mx-auto" ]
        [ case model.userGroup of
            Initial ->
                text ""

            Loading ->
                Html.div []
                    [ Html.span [ Attr.class "loader" ] []
                    ]

            Error error ->
                Html.div [ Attr.class "flex items-center z-12 bg-red-200 mt-10 border px-3 py-2 mx-4 rounded border-red-400 text-2xl text-red-400 " ]
                    [ Html.span [ Attr.class "w-[30px] flex mr-2" ] [ Icons.cautionIcon ]
                    , Html.p [] [ text error ]
                    ]

            Success userGroup ->
                Html.div [ Attr.class "flex flex-col mx-4 md:mx-0 mt-16 mb-48" ]
                    [ Html.div [ Attr.class "mx-4 md:mx-0" ]
                        [ Html.div [ Attr.class "flex w-[140px] h-[30px]" ]
                            [ Html.img [ Attr.src "logo.png" ] []
                            ]
                        , Html.h1 [ Attr.class "text-3xl  md:text-5xl mb-10" ] [ text "User Group Settings Form" ]
                        ]
                    , if model.isFormSent then
                        Html.div [ Attr.class "flex mb-4 text-green-400 rounded border border-green-400 bg-green-200 px-3 py-2" ]
                            [ Html.span [ Attr.class "mr-2 flex w-[20px]" ] [ Icons.checkIcon ]
                            , Html.p [] [ text "Success ! Form has been sent !" ]
                            ]

                      else
                        text ""
                    , Html.div [ Attr.class "" ]
                        [ Html.div [ Attr.class "flex flex-col" ]
                            [ Html.div [ Attr.class "" ]
                                [ viewTabs model.tabActive ]
                            , Html.div
                                [ Attr.class "bg-blue-100 p-4 mb-10 overflow-hidden" ]
                                [ Html.div [ Attr.class "mb-4" ] (model.errors |> Set.toList |> List.map (\error -> viewErrorMessage error))
                                , case model.tabActive of
                                    SettingsTab ->
                                        viewSettings model.formData userGroup.settings.isInherited model.showTooltip

                                    DetailsTab ->
                                        viewContactDetails model.formData { isInherited = userGroup.contactDetails.isInherited, prefMethod = model.formData.prefMethod } model.toggleDropdown

                                    TagsTab ->
                                        viewTags model.formData.tags model.formData.inputFields
                                ]
                            ]
                        , Html.button [ onClick <| SubmitForm { isSettingsInherited = userGroup.settings.isInherited, isDetailsInherited = userGroup.contactDetails.isInherited }, Attr.class "flex font-semi-bold px-5 py-2 rounded-full bg-blue-400 hover:bg-blue-300 active:bg-blue-500 easy-in-out transition-all text-white" ] [ Html.span [ Attr.class "mr-2 flex w-[20px]" ] [ Icons.checkIcon ], text "Submit" ]
                        ]
                    ]
        ]


viewTabs : Tabs -> Html Msg
viewTabs tabActive =
    Html.ul [ Attr.class "flex text-xl md:text-3xl cursor-pointer" ]
        [ viewTab { isActive = tabActive == SettingsTab, toMsg = ActiveTab SettingsTab, txtValue = "Settings" }
        , viewTab { isActive = tabActive == DetailsTab, toMsg = ActiveTab DetailsTab, txtValue = "Details" }
        , viewTab { isActive = tabActive == TagsTab, toMsg = ActiveTab TagsTab, txtValue = "Tags" }
        ]


viewTab : { isActive : Bool, toMsg : Msg, txtValue : String } -> Html Msg
viewTab { isActive, toMsg, txtValue } =
    Html.li
        [ Attr.class "p-4"
        , Attr.class
            (if isActive then
                "bg-blue-100"

             else
                "bg-white"
            )
        , onClick toMsg
        ]
        [ text txtValue ]


viewTags : Tags -> Dict String String -> Html Msg
viewTags tags inputFields =
    Html.section []
        [ Html.p [ Attr.class "mb-10" ] [ text "Add new tags, name limit is 33 characters and it should be unique" ]
        , Html.div [ Attr.class "mb-10" ]
            [ viewFieldset
                { id = NewTagNameId
                , daysString = Nothing
                , requiredField = False
                , textValue = "Tag Name"
                , inputValue = inputFields |> Dict.get "newTagName" |> Maybe.withDefault ""
                , maxWidthFieldClass = "w-[85px]"
                , overrideClass = Nothing
                }
            , viewFieldset
                { id = NewTagValId
                , daysString = Nothing
                , requiredField = False
                , textValue = "Tag Value"
                , inputValue = inputFields |> Dict.get "newTagValue" |> Maybe.withDefault ""
                , maxWidthFieldClass = "w-[85px]"
                , overrideClass = Nothing
                }
            , Html.button
                [ onClick AddNewTag, Attr.class "font-semi-bold flex items-center px-2 py-1 rounded-full bg-blue-400  hover:bg-blue-300 active:bg-blue-500 transition-all text-white" ]
                [ Html.span [ Attr.class "w-[20px] flex" ] [ Icons.plusIcon ]
                , text "Add tag"
                ]
            ]
        , Html.ul [ Attr.class "flex gap-4 flex-wrap" ]
            (tags
                |> Dict.toList
                |> List.map
                    (\( tagName, value ) ->
                        Html.li [ Attr.class "flex bg-blue-200 text-xs text-blue-400 easy-in-out border border-blue-400 rounded" ]
                            [ Html.span [ Attr.class "px-2 py-1" ] [ text <| tagName ++ ": " ++ value ]
                            , Html.span [ onClick <| DeleteTag tagName, Attr.class "bg-red-400 cursor-pointer text-red-700 px-2 py-1 text-xs" ]
                                [ Html.span [ Attr.class "flex w-[14px]" ] [ Icons.deleteIcon ] ]
                            , Html.span [ onClick <| EditTag tagName, Attr.class "bg-gray-300 cursor-pointer text-gray-600 px-2 py-1 text-xs rounded-r" ]
                                [ Html.span [ Attr.class "flex w-[14px]" ] [ Icons.editIcon ] ]
                            ]
                    )
            )
        ]


viewSettings : FormData -> Bool -> Bool -> Html Msg
viewSettings { inputFields, shouldTrash } isInherited showTooltip =
    Html.section [ Attr.class "relative" ]
        [ if isInherited then
            viewDisablePageOverlay

          else
            text ""
        , Html.p [ Attr.class "mb-10" ] [ text "How many days do you want document to stay in a certain state ?" ]
        , formWrapper
            [ viewFieldset
                { id = PrepId
                , daysString = inputFields |> Dict.get "preparation"
                , requiredField = False
                , textValue = "Preparation"
                , inputValue = inputFields |> Dict.get "preparation" |> Maybe.withDefault ""
                , maxWidthFieldClass = "w-[94px]"
                , overrideClass = Just "mr-4 w-[45px]"
                }
            , viewFieldset
                { id = CloseId
                , daysString = inputFields |> Dict.get "closed"
                , requiredField = False
                , textValue = "Closed"
                , inputValue = inputFields |> Dict.get "closed" |> Maybe.withDefault ""
                , maxWidthFieldClass = "w-[94px]"
                , overrideClass = Just "mr-4 w-[45px]"
                }
            , viewFieldset
                { id = CancelId
                , daysString = inputFields |> Dict.get "canceled"
                , requiredField = False
                , textValue = "Canceled"
                , inputValue = inputFields |> Dict.get "canceled" |> Maybe.withDefault ""
                , maxWidthFieldClass = "w-[94px]"
                , overrideClass = Just "mr-4 w-[45px]"
                }
            , viewFieldset
                { id = TimeOutId
                , daysString = inputFields |> Dict.get "timedOut"
                , requiredField = False
                , textValue = "Timed out"
                , inputValue = inputFields |> Dict.get "timedOut" |> Maybe.withDefault ""
                , maxWidthFieldClass = "w-[94px]"
                , overrideClass = Just "mr-4 w-[45px]"
                }
            , viewFieldset
                { id = ErrId
                , daysString = inputFields |> Dict.get "error"
                , requiredField = False
                , textValue = "Error"
                , inputValue = inputFields |> Dict.get "error" |> Maybe.withDefault ""
                , maxWidthFieldClass = "w-[94px]"
                , overrideClass = Just "mr-4 w-[45px]"
                }
            , Html.fieldset []
                [ Html.label [ Attr.class "flex items-center", Attr.for "shouldTrash" ]
                    [ Html.span [ Attr.class "flex items-center w-[94px]" ]
                        [ text "Destroy"
                        , viewInfoIcon "You confirm that item can be deleted after defined amount od days" showTooltip
                        ]
                    , Html.input [ onCheck ToggleCheckbox, Attr.class "ml-4 w-[20px] h-[20px] border-0 text-blue-400 shadow-sm ring-transparent focus:ring-0 focus:ring-offset-0 cursor-pointer border-blue-400,", Attr.type_ "checkbox", Attr.id "shouldTrash", Attr.checked shouldTrash ] []
                    ]
                ]
            ]
        ]


viewFieldset :
    { id : Ids
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
        [ Html.label [ Attr.class "flex items-center", Attr.for (fromIdToString id) ]
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


viewInputText : { id : Ids, value : String, overrideClass : Maybe String } -> Html Msg
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
        , Attr.id (fromIdToString id)
        , onInput (id |> OnInputChange)
        , Attr.value value
        ]
        []


viewContactDetails : FormData -> { isInherited : Bool, prefMethod : PreferredContactMethod } -> Bool -> Html Msg
viewContactDetails { inputFields } { isInherited, prefMethod } shouldShowDropdown =
    Html.section [ Attr.class "relative" ]
        [ if isInherited then
            viewDisablePageOverlay

          else
            text ""
        , Html.p [ Attr.class "mb-10" ] [ text "User's contact details" ]
        , formWrapper
            [ Html.div [ Attr.class "flex mb-10 items-center" ]
                [ Html.div [] [ text "Choose preferred contact method" ]
                , viewPreferredMethodDropdown { prefMethod = prefMethod, shouldShowDropdown = shouldShowDropdown }
                ]
            , viewFieldset
                { id = EmailId
                , daysString = Nothing
                , requiredField = prefMethod == Email
                , textValue = "Email"
                , inputValue = inputFields |> Dict.get "email" |> Maybe.withDefault ""
                , maxWidthFieldClass = "w-[132px]"
                , overrideClass =
                    if prefMethod == Email then
                        Just "rounded-t-none ring-blue-400 ring-1"

                    else
                        Nothing
                }
            , viewFieldset
                { id = PhoneId
                , daysString = Nothing
                , requiredField = prefMethod == Phone
                , textValue = "Phone"
                , inputValue = inputFields |> Dict.get "phone" |> Maybe.withDefault ""
                , maxWidthFieldClass = "w-[132px]"
                , overrideClass =
                    if prefMethod == Phone then
                        Just "rounded-t-none ring-blue-400 ring-1"

                    else
                        Nothing
                }
            , viewFieldset
                { id = CompanyId
                , daysString = Nothing
                , requiredField = False
                , textValue = "Company Name"
                , inputValue = inputFields |> Dict.get "companyName" |> Maybe.withDefault ""
                , maxWidthFieldClass = "w-[132px]"
                , overrideClass = Nothing
                }
            , viewFieldset
                { id = PostId
                , daysString = Nothing
                , requiredField = prefMethod == Post
                , textValue = "Address"
                , inputValue = inputFields |> Dict.get "address" |> Maybe.withDefault ""
                , maxWidthFieldClass = "w-[132px]"
                , overrideClass =
                    if prefMethod == Post then
                        Just "rounded-t-none ring-blue-400 ring-1"

                    else
                        Nothing
                }
            , viewFieldset
                { id = ZipId
                , daysString = Nothing
                , requiredField = False
                , textValue = "Zip"
                , inputValue = inputFields |> Dict.get "zip" |> Maybe.withDefault ""
                , maxWidthFieldClass = "w-[132px]"
                , overrideClass = Nothing
                }
            , viewFieldset
                { id = CityId
                , daysString = Nothing
                , requiredField = False
                , textValue = "City"
                , inputValue = inputFields |> Dict.get "city" |> Maybe.withDefault ""
                , maxWidthFieldClass = "w-[132px]"
                , overrideClass = Nothing
                }
            , viewFieldset
                { id = CountryId
                , daysString = Nothing
                , requiredField = False
                , textValue = "Country"
                , inputValue = inputFields |> Dict.get "countryName" |> Maybe.withDefault ""
                , maxWidthFieldClass = "w-[132px]"
                , overrideClass = Nothing
                }
            ]
        ]


viewDisablePageOverlay : Html Msg
viewDisablePageOverlay =
    Html.div []
        [ Html.div [ Attr.class "mb-4" ]
            [ viewErrorMessage "It's not possible to modify data on this page" ]
        , Html.div
            [ Attr.id "overlay"
            , Attr.class "bg-blue-100 opacity-60 w-[100%] h-[100%] absolute z-10"
            ]
            []
        ]


viewErrorMessage : String -> Html Msg
viewErrorMessage txt =
    Html.div [ Attr.class "flex items-center z-12 text-xs text-red-400 " ]
        [ Html.span [ Attr.class "w-[20px] flex mr-2" ] [ Icons.cautionIcon ]
        , Html.p [] [ text txt ]
        ]


viewPreferredMethodDropdown : { prefMethod : PreferredContactMethod, shouldShowDropdown : Bool } -> Html Msg
viewPreferredMethodDropdown { prefMethod, shouldShowDropdown } =
    Html.div [ Attr.class "ml-4 relative cursor-pointer" ]
        [ Html.div
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
