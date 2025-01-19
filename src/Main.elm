module Main exposing (main)

import Browser
import Browser.Dom as Dom
import Data exposing (userGroup)
import Form exposing (Form, getChangedFields)
import Form.Error as Error exposing (ErrorValue(..))
import Form.Field as Field
import Form.Input as Input exposing (selectInput)
import Form.Validate as Validation exposing (Validation, bool, customError, customValidation, emptyString, fail, field, format, int, list, maxInt, maxLength, minInt, oneOf, succeed, withCustomError)
import Html exposing (Html, button, div, fieldset, h1, label, legend, p, span, text)
import Html.Attributes as Attrs exposing (attribute, class, disabled, for, id, placeholder, readonly, title, type_)
import Html.Attributes.Extra exposing (attributeIf)
import Html.Events exposing (onClick)
import Html.Extra exposing (viewIf, viewMaybe)
import Json.Decode as Decode exposing (Decoder)
import List.Extra as List
import Maybe
import Regex
import Set
import String.Extra exposing (isBlank)
import Task



---- MODEL ----


type alias Tag =
    { name : String
    , value : String
    }


type alias UserGroupChild =
    { id : String
    , name : String
    }


type alias DataRetentionPolicy =
    { idleDocTimeoutPreparation : Maybe Int
    , idleDocTimeoutClosed : Maybe Int
    , idleDocTimeoutCanceled : Maybe Int
    , idleDocTimeoutTimedout : Maybe Int
    , idleDocTimeoutRejected : Maybe Int
    , idleDocTimeoutError : Maybe Int
    , isImmediatelyTrashed : Bool
    }


type ContactMethod
    = Email
    | Phone
    | Post


type alias ContactDetails =
    { name : String
    , phone : String
    , email : String
    , address : String
    , zip : String
    , city : String
    , country : String
    , preferredContactMethod : ContactMethod
    }


type alias UserGroupInheritanceDetails =
    { parentId : String
    , settingsInheritedFrom : String
    , contactDetailsInheritedFrom : String
    }


type UserGroupInheritance
    = Root
    | Child UserGroupInheritanceDetails


type alias UserGroup =
    { id : String
    , inheritance : UserGroupInheritance
    , name : String
    , children : List UserGroupChild
    , settings : DataRetentionPolicy
    , contactDetails : ContactDetails
    , tags : List Tag
    }


type alias UserGroupFormInput =
    { settings : DataRetentionPolicy

    -- This field is supposed to be part of settings, but to avoid making an extension of DataRetentionPolicy - it is kept on the same level as settings.
    -- However, in the tree structure of the Form fields from elm-form it is used as a field of settings group.
    , newDataRetentionSetting : SettingsFormField
    , contactDetails : ContactDetails
    , tags : List Tag
    }


type alias Page =
    { userGroup : UserGroup
    , form : Form String UserGroupFormInput
    , isGroupExpanded : List ( GroupField, Bool )
    , displayedDataRetentionDaySettings : List SettingsFormField
    , allDataRetentionDaySettings : List SettingsFormField
    }


type Model
    = Loaded Page
    | LoadingFailed String


init : ( Model, Cmd Msg )
init =
    case Decode.decodeString userGroupDecoder userGroup of
        Ok data ->
            ( let
                ( formFields, displayedSettings ) =
                    initialFields data
              in
              Loaded
                { userGroup = data
                , form = Form.initial formFields validateUserGroupForm
                , allDataRetentionDaySettings = dataRetentionDayFields data.settings |> List.map Tuple.first
                , displayedDataRetentionDaySettings = displayedSettings
                , isGroupExpanded = [ SettingsGroup, ContactDetailsGroup, TagsGroup ] |> List.map (\g -> ( g, False ))
                }
            , Cmd.none
            )

        Err err ->
            ( err |> Decode.errorToString |> LoadingFailed, Cmd.none )



---- UPDATE ----


type Msg
    = NoOp
    | FormMsg Form.Msg
    | AddDataRetentionSetting SettingsFormField
    | RemoveDataRetentionSetting SettingsFormField
    | ToggleGroupExpansion GroupField Bool
    | ResetForm


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case model of
        LoadingFailed _ ->
            ( model, Cmd.none )

        Loaded page ->
            case msg of
                NoOp ->
                    ( Loaded page, Cmd.none )

                FormMsg formMsg ->
                    case ( formMsg, Form.getOutput page.form ) of
                        ( Form.Submit, Just userGroupFormInput ) ->
                            let
                                updatedUserGroup =
                                    page.userGroup
                                        |> (\u ->
                                                { u
                                                    | settings = userGroupFormInput.settings
                                                    , contactDetails = userGroupFormInput.contactDetails
                                                    , tags = userGroupFormInput.tags
                                                }
                                           )

                                ( formFields, displayedSettings ) =
                                    initialFields updatedUserGroup
                            in
                            ( Loaded
                                { page
                                    | userGroup = updatedUserGroup
                                    , form = Form.initial formFields validateUserGroupForm
                                    , displayedDataRetentionDaySettings = displayedSettings
                                }
                            , Cmd.none
                            )

                        _ ->
                            ( Loaded { page | form = Form.update validateUserGroupForm formMsg page.form }, Cmd.none )

                AddDataRetentionSetting fieldType ->
                    let
                        updatedDisplayedSettings =
                            page.displayedDataRetentionDaySettings ++ [ fieldType ]

                        updatedHiddenSettings =
                            listDiff updatedDisplayedSettings page.allDataRetentionDaySettings
                    in
                    ( Loaded
                        { page
                            | form =
                                Form.update validateUserGroupForm
                                    (Form.Input (composeSettingsFieldPath SelectedNewSettingField)
                                        Form.Text
                                        (updatedHiddenSettings |> List.head |> Maybe.map settingsFieldToString |> Maybe.withDefault "" |> Field.String)
                                    )
                                    page.form
                            , displayedDataRetentionDaySettings = updatedDisplayedSettings
                        }
                    , Task.attempt (\_ -> NoOp) (Dom.focus (composeSettingsFieldPath fieldType))
                    )

                RemoveDataRetentionSetting fieldType ->
                    let
                        updatedDisplayedSettings =
                            List.remove fieldType page.displayedDataRetentionDaySettings

                        updatedHiddenSettings =
                            listDiff updatedDisplayedSettings page.allDataRetentionDaySettings
                    in
                    ( Loaded
                        { page
                            | form =
                                Form.update validateUserGroupForm
                                    (Form.Input (composeSettingsFieldPath SelectedNewSettingField)
                                        Form.Text
                                        (updatedHiddenSettings |> List.head |> Maybe.map settingsFieldToString |> Maybe.withDefault "" |> Field.String)
                                    )
                                    page.form
                                    |> Form.update validateUserGroupForm
                                        (Form.Input (composeSettingsFieldPath fieldType)
                                            Form.Text
                                            (Field.String "")
                                        )
                            , displayedDataRetentionDaySettings = updatedDisplayedSettings
                        }
                    , Cmd.none
                    )

                ToggleGroupExpansion group isExpanded ->
                    ( Loaded { page | isGroupExpanded = List.updateIf (\( g, _ ) -> g == group) (\( g, _ ) -> ( g, isExpanded |> not )) page.isGroupExpanded }, Cmd.none )

                ResetForm ->
                    let
                        ( formFields, displayedSettings ) =
                            initialFields page.userGroup
                    in
                    ( Loaded
                        { page
                            | form = Form.update validateUserGroupForm (Form.Reset formFields) page.form
                            , displayedDataRetentionDaySettings = displayedSettings
                        }
                    , Cmd.none
                    )



---- VIEW ----


view : Model -> Html Msg
view model =
    case model of
        LoadingFailed error ->
            div [ class "flex flex-col w-[1024px] items-center mx-auto mt-16 mb-48" ]
                [ span [ class "text-xl" ] [ text "An error occurred: " ]
                , span [ class "text-red-700" ] [ text error ]
                ]

        Loaded page ->
            div [ class "flex flex-col w-11/12 md:w-3/6 xl:w-2/6 items-center mx-auto mt-5 mb-10" ]
                [ div [ class "w-full" ]
                    [ div [ class "flex flex-row justify-between pt-2 bg-white", class "sticky top-0" ]
                        [ h1 [ class "text-4xl mb-5 text-center" ] [ text "User Group" ]
                        , viewIf (getChangedFields page.form |> Set.isEmpty |> not)
                            (div []
                                [ button
                                    [ type_ "button"
                                    , secondaryButtonAttributes
                                    , onClick ResetForm
                                    ]
                                    [ text "Cancel" ]
                                , button
                                    [ type_ "button"
                                    , class "border bg-blue-500 text-white hover:bg-blue-400 rounded-lg py-2 px-2 lg:px-3 ml-2 mb-5"
                                    , onClick Form.Submit
                                    ]
                                    [ text "Save" ]
                                    |> Html.map FormMsg
                                ]
                            )
                        ]
                    , .settingsInheritedFrom |> isFormReadonly page.userGroup.inheritance |> settingsFormView page
                    , .contactDetailsInheritedFrom |> isFormReadonly page.userGroup.inheritance |> contactDetailsView page
                    , tagsView page
                    ]
                ]


settingsFormView : Page -> Bool -> Html Msg
settingsFormView page isReadonly =
    let
        getSettingsField fieldType =
            Form.getFieldAsString (composeSettingsFieldPath fieldType) page.form

        daysInput fieldType =
            labeledFieldInput
                (getSettingsField fieldType)
                (div [ class "flex flex-row" ]
                    [ Input.baseInput
                        "number"
                        Field.String
                        Form.Text
                        (getSettingsField fieldType)
                        (inputAttributes (getSettingsField fieldType) "number of days" isReadonly
                            ++ [ minRetentionDays |> String.fromInt |> Attrs.min, maxRetentionDays |> String.fromInt |> Attrs.max ]
                        )
                        |> Html.map FormMsg
                    , div [ class "my-auto ml-2" ] [ text "day(s)" ]
                    , div [ class "my-auto ml-2" ]
                        [ button [ type_ "button", class "fas fa-trash text-red-600", RemoveDataRetentionSetting fieldType |> onClick ] [] ]
                        |> viewIf (not isReadonly)
                    ]
                )
                (settingsFieldToString fieldType)
                |> viewIf (List.member fieldType page.displayedDataRetentionDaySettings || isReadonly)

        isExpanded =
            isGroupExpanded page.isGroupExpanded SettingsGroup

        ( labelText, ariaExpandedAttribute ) =
            toggleExpandButtonDetails isExpanded
    in
    Html.form [ class "border border-gray-200 rounded-lg px-5 pb-2 mb-5" ]
        [ fieldset []
            [ legend [ class "w-full flex flex-row justify-between mb-2 pt-5" ]
                [ span [ class "text-xl md:text-2xl lg:text-3xl" ]
                    [ text "Data Retention Settings"
                    , Form.getFieldAsString (groupFieldToName SettingsGroup) page.form |> .error |> showIfError
                    ]
                , button
                    [ class "ml-auto"
                    , ariaExpandedAttribute
                    , attribute "aria-controls" (groupFieldToName SettingsGroup)
                    , type_ "button"
                    , secondaryButtonAttributes
                    , ToggleGroupExpansion SettingsGroup isExpanded |> onClick
                    ]
                    [ text labelText ]
                ]
            , viewIf isExpanded
                (div [ id (groupFieldToName SettingsGroup) ]
                    [ daysInput PreparationField
                    , daysInput ClosedField
                    , daysInput CanceledField
                    , daysInput TimedOutField
                    , daysInput RejectedField
                    , daysInput ErrorField
                    , viewMaybe
                        (\newSetting ->
                            viewIf (not isReadonly)
                                (div [ class "flex flex-row" ]
                                    [ selectInput
                                        (listDiff page.displayedDataRetentionDaySettings page.allDataRetentionDaySettings
                                            |> List.map (\fieldType -> ( settingsFieldToString fieldType, settingsFieldToString fieldType ))
                                        )
                                        (getSettingsField SelectedNewSettingField)
                                        [ class "block border border-gray-300 text-gray-900 rounded-lg p-2" ]
                                        |> Html.map FormMsg
                                    , (SelectedNewSettingField |> getSettingsField |> .error) |> showIfError
                                    , button
                                        [ type_ "button"
                                        , secondaryButtonAttributes
                                        , AddDataRetentionSetting newSetting |> onClick
                                        ]
                                        [ div [ class "line-clamp-1 break-all" ] [ ("Add new setting for " ++ settingsFieldToString newSetting) |> text ] ]
                                    ]
                                )
                        )
                        (SelectedNewSettingField |> getSettingsField |> .value |> Maybe.withDefault "" |> stringToSettingsField)
                    , labeledFieldInput
                        (ImmediateTrashField |> getSettingsField)
                        (Input.checkboxInput
                            (Form.getFieldAsBool (composeSettingsFieldPath ImmediateTrashField) page.form)
                            [ ImmediateTrashField |> composeSettingsFieldPath |> id
                            , class "block border border-gray-300 text-gray-900 rounded-lg p-2 h-4 w-4 disabled:bg-gray-300 disabled:cursor-not-allowed outline-0"
                            , disabled isReadonly
                            , attributeIf isReadonly titleForInherited
                            ]
                            |> Html.map FormMsg
                        )
                        (settingsFieldToString ImmediateTrashField)
                    ]
                )
            ]
        ]


contactDetailsView : Page -> Bool -> Html Msg
contactDetailsView page isReadonly =
    let
        contactDetailsTextInput field labelText placeholder_ =
            labeledFieldInput
                field
                (Input.textInput
                    field
                    (inputAttributes field placeholder_ isReadonly)
                    |> Html.map FormMsg
                )
                labelText

        getContactDetailsField fieldType =
            Form.getFieldAsString (composeContactDetailsFieldPath fieldType) page.form

        preferredContactMethodField =
            getContactDetailsField PreferredContactMethodField

        isExpanded =
            isGroupExpanded page.isGroupExpanded ContactDetailsGroup

        ( collapseButtonText, ariaExpandedAttribute ) =
            toggleExpandButtonDetails isExpanded
    in
    Html.form [ class "border border-gray-200 rounded-lg px-5 pb-2 mb-5" ]
        [ fieldset []
            [ legend [ class "w-full flex flex-row justify-between mb-2 pt-5" ]
                [ span [ class "text-xl md:text-2xl lg:text-3xl" ]
                    [ text "Contact Details"
                    , Form.getFieldAsString (groupFieldToName ContactDetailsGroup) page.form |> .error |> showIfError
                    ]
                , button
                    [ class "ml-auto"
                    , ariaExpandedAttribute
                    , attribute "aria-controls" (groupFieldToName ContactDetailsGroup)
                    , type_ "button"
                    , secondaryButtonAttributes
                    , ToggleGroupExpansion ContactDetailsGroup isExpanded |> onClick
                    ]
                    [ text collapseButtonText ]
                ]
            , viewIf isExpanded
                (div [ id (groupFieldToName SettingsGroup) ]
                    [ contactDetailsTextInput (getContactDetailsField CompanyNameField) "Company name" "Company name"
                    , contactDetailsTextInput (getContactDetailsField PhoneField) "Phone" "+46000000000"
                    , contactDetailsTextInput (getContactDetailsField EmailField) "Email" "test@test.com"
                    , contactDetailsTextInput (getContactDetailsField AddressField) "Address" "Address"
                    , contactDetailsTextInput (getContactDetailsField ZipField) "Zip" "Zip code"
                    , contactDetailsTextInput (getContactDetailsField CityField) "City" "City"
                    , contactDetailsTextInput (getContactDetailsField CountryField) "Country" "Country"
                    , labeledFieldInput
                        preferredContactMethodField
                        (div [ class "flex flex-col justify-self-start" ]
                            (List.map
                                (\( radioOption, labelText ) ->
                                    let
                                        optionId =
                                            preferredContactMethodField.path ++ "." ++ contactMethodToString radioOption
                                    in
                                    div [ class "flex flex-row py-1 px-1" ]
                                        [ Input.radioInput
                                            (contactMethodToString radioOption)
                                            preferredContactMethodField
                                            [ id optionId
                                            , disabled isReadonly
                                            , attributeIf isReadonly titleForInherited
                                            ]
                                            |> Html.map FormMsg
                                        , label [ for optionId, class "block text-gray-900 mb-1 ml-2" ] [ text labelText ]
                                        ]
                                )
                                ([ Post, Email, Phone ] |> List.map (\v -> ( v, contactMethodToString v )))
                            )
                        )
                        "Preferred contact method"
                    ]
                )
            ]
        ]


tagsView : Page -> Html Msg
tagsView page =
    let
        getTagField fieldType index =
            Form.getFieldAsString (composeTagsFieldPath fieldType index) page.form

        tagsGroupName =
            groupFieldToName TagsGroup

        viewTag tagIndex =
            div [ class "flex flex-row" ]
                [ div [ class "border border-gray-200 rounded-lg px-2 lg:px-3 my-3 w-full" ]
                    [ labeledFieldInput
                        (getTagField TagNameField tagIndex)
                        (Input.textInput
                            (getTagField TagNameField tagIndex)
                            (inputAttributes (getTagField TagNameField tagIndex) "Name of the tag" False)
                        )
                        "Name"
                    , labeledFieldInput
                        (getTagField TagValueField tagIndex)
                        (Input.textInput
                            (getTagField TagValueField tagIndex)
                            (inputAttributes (getTagField TagValueField tagIndex) "Name of the tag" False)
                        )
                        "Value"
                    ]
                , div [ class "my-auto ml-2" ]
                    [ button [ type_ "button", class "fas fa-trash text-red-600", Form.RemoveItem tagsGroupName tagIndex |> onClick ] [] ]
                ]
                |> Html.map FormMsg

        tagIndexes =
            Form.getListIndexes tagsGroupName page.form

        isExpanded =
            isGroupExpanded page.isGroupExpanded TagsGroup

        ( collapseButtonText, ariaExpandedAttribute ) =
            toggleExpandButtonDetails isExpanded
    in
    Html.form [ class "border border-gray-200 rounded-lg px-5 pb-2" ]
        [ fieldset []
            [ legend [ class "w-full flex flex-row justify-between mb-2 pt-5" ]
                [ span [ class "text-xl md:text-2xl lg:text-3xl" ]
                    [ text "Tags"
                    , Form.getFieldAsString (groupFieldToName TagsGroup) page.form |> .error |> showIfError
                    ]
                , button
                    [ class "ml-auto"
                    , ariaExpandedAttribute
                    , attribute "aria-controls" tagsGroupName
                    , type_ "button"
                    , secondaryButtonAttributes
                    , ToggleGroupExpansion TagsGroup isExpanded |> onClick
                    ]
                    [ text collapseButtonText ]
                ]
            , viewIf isExpanded
                (div [ id tagsGroupName ]
                    (List.map viewTag tagIndexes
                        ++ [ button
                                [ type_ "button"
                                , secondaryButtonAttributes
                                , (groupFieldToName TagsGroup |> Form.Append) |> onClick
                                ]
                                [ text "Add new tag" ]
                                |> Html.map FormMsg
                           ]
                    )
                )
            ]
        ]


{-| From "is expanded" state gets:

  - labelText that can be displayed on the button to Expand or Collapse the section.
  - aria-expanded attribute to provide information about the state to the accessibility tools.

-}
toggleExpandButtonDetails : Bool -> ( String, Html.Attribute msg )
toggleExpandButtonDetails isExpanded =
    if isExpanded then
        ( "Collapse", attribute "aria-expanded" "true" )

    else
        ( "Expand", attribute "aria-expanded" "false" )


{-| Takes in a list of all groups of frields and their state (expanded or not) and determines if the provided
group of fields is expanded.
-}
isGroupExpanded : List ( GroupField, Bool ) -> GroupField -> Bool
isGroupExpanded isGroupExpanded_ group =
    isGroupExpanded_ |> List.find (\( g, _ ) -> g == group) |> Maybe.map Tuple.second |> Maybe.withDefault False


{-| Verifies if the form (part) is readonly from the provided User Group inheritence.
For User Group that has a parent - verifies if the relevant part of User Group is inherited (using supplied function to access it),
and if it is inherited - it is readonly.
Root is always non-readonly, since it cannot be inherited.
-}
isFormReadonly : UserGroupInheritance -> (UserGroupInheritanceDetails -> String) -> Bool
isFormReadonly inheritance getInheritedFrom =
    case inheritance of
        Root ->
            False

        Child inheritanceDetails ->
            inheritanceDetails |> getInheritedFrom |> String.isEmpty |> not


labeledFieldInput : Form.FieldState String String -> Html msg -> String -> Html msg
labeledFieldInput field content labelText =
    div [ class "my-4" ]
        [ label [ for field.path, class "block mb-1 text-gray-900" ] [ text labelText ]
        , content
        , showIfError field.error
        ]


{-| Provides list of attributes that can be used for form inputs.

  - Highlights field with red in case of an error.
  - Marks field as readonly and gives a user-friendly tooltip to provide information about field being inherited.
  - Displays a placeholder if field is not filled in.

-}
inputAttributes : Form.FieldState String String -> String -> Bool -> List (Html.Attribute msg)
inputAttributes field placeholder_ isReadonly =
    [ id field.path
    , class "block border border-gray-300 text-gray-900 rounded-lg p-2 read-only:bg-gray-300 read-only:cursor-not-allowed outline-0 w-full"
    , placeholder placeholder_
    , readonly isReadonly
    , attributeIf isReadonly titleForInherited
    , attributeIf (field.error |> errorMessage |> String.isEmpty |> not) (class "border-red-500")
    ]


secondaryButtonAttributes : Html.Attribute msg
secondaryButtonAttributes =
    class "border border-gray-500 bg-white text-grey-900 hover:bg-gray-700 hover:text-white rounded-lg py-2 px-2 lg:px-3 my-auto ml-2"


titleForInherited : Html.Attribute msg
titleForInherited =
    title "Field cannot be edited because it is inherited from another User Group."


{-| Displays a small sized error message in red in case provided Maybe error contains an error.
-}
showIfError : Maybe (ErrorValue String) -> Html msg
showIfError error =
    viewIf
        (error |> errorMessage |> String.isEmpty |> not)
        (p [ class "pt-1 ps-1 text-xs text-red-600" ]
            [ error |> errorMessage |> text ]
        )


{-| Returns the error message text. Returns an empty string if there is no error.
-}
errorMessage : Maybe (ErrorValue String) -> String
errorMessage maybeError =
    maybeError |> Maybe.map forErrorToString |> Maybe.withDefault ""



---- PROGRAM ----


main : Program () Model Msg
main =
    Browser.application
        { view =
            \model ->
                { title = "Scrive elm challenge task solution"
                , body = [ view model ]
                }
        , init = \_ _ _ -> init
        , update = update
        , subscriptions = always Sub.none
        , onUrlRequest = always NoOp
        , onUrlChange = always NoOp
        }



---- FORM FIELDS AND GROUPS ----
{- These FormField and GroupField types are created to mitigate a risk of typos in keys of Scrive elm-form library's fields,
   since typos are possible if you use hardcoded strings to access the fields.
   Same applies to all \*FormField types below.
-}


type SettingsFormField
    = PreparationField
    | ClosedField
    | CanceledField
    | TimedOutField
    | RejectedField
    | ErrorField
    | ImmediateTrashField
    | SelectedNewSettingField


type ContactDetailsFormField
    = CompanyNameField
    | PhoneField
    | EmailField
    | AddressField
    | ZipField
    | CityField
    | CountryField
    | PreferredContactMethodField


type TagsFormField
    = TagNameField
    | TagValueField


type GroupField
    = SettingsGroup
    | ContactDetailsGroup
    | TagsGroup


{-| Converts settings field to a string, returning its name in the Form.
Please note that this is a name, not a path.
-}
settingsFormFieldToName : SettingsFormField -> String
settingsFormFieldToName formField =
    case formField of
        PreparationField ->
            "preparation"

        ClosedField ->
            "closed"

        CanceledField ->
            "canceled"

        TimedOutField ->
            "timedout"

        RejectedField ->
            "rejected"

        ErrorField ->
            "error"

        ImmediateTrashField ->
            "immediatetrash"

        SelectedNewSettingField ->
            "selectednewsetting"


{-| Converts contact details field to a string, returning its name in the Form.
Please note that this is a name, not a path.
-}
contactDetailsFormFieldToName : ContactDetailsFormField -> String
contactDetailsFormFieldToName formField =
    case formField of
        CompanyNameField ->
            "companyname"

        PhoneField ->
            "phone"

        EmailField ->
            "email"

        AddressField ->
            "address"

        ZipField ->
            "zip"

        CityField ->
            "city"

        CountryField ->
            "country"

        PreferredContactMethodField ->
            "preferredcontactmethod"


{-| Converts tags field to a string, returning its name in the Form.
Please note that this is a name, not a path.
-}
tagsFormFieldToName : TagsFormField -> String
tagsFormFieldToName formField =
    case formField of
        TagNameField ->
            "name"

        TagValueField ->
            "value"


{-| Converts group field to a string, returning its name in the Form.
-}
groupFieldToName : GroupField -> String
groupFieldToName group =
    case group of
        SettingsGroup ->
            "settings"

        ContactDetailsGroup ->
            "contactdetails"

        TagsGroup ->
            "tags"


{-| Composes the full path to a field in settings group.
-}
composeSettingsFieldPath : SettingsFormField -> String
composeSettingsFieldPath field =
    groupFieldToName SettingsGroup ++ "." ++ settingsFormFieldToName field


{-| Composes the full path to a field in contact details group.
-}
composeContactDetailsFieldPath : ContactDetailsFormField -> String
composeContactDetailsFieldPath field =
    groupFieldToName ContactDetailsGroup ++ "." ++ contactDetailsFormFieldToName field


{-| Composes the full path to a field in tags, including the index of the tag in the list of tags in the form.
-}
composeTagsFieldPath : TagsFormField -> Int -> String
composeTagsFieldPath field index =
    groupFieldToName TagsGroup ++ "." ++ String.fromInt index ++ "." ++ tagsFormFieldToName field


contactMethodToString : ContactMethod -> String
contactMethodToString method =
    case method of
        Email ->
            "Email"

        Phone ->
            "Phone"

        Post ->
            "Post"


stringToContactMethod : String -> Maybe ContactMethod
stringToContactMethod str =
    case str of
        "Email" ->
            Just Email

        "Phone" ->
            Just Phone

        "Post" ->
            Just Post

        _ ->
            Nothing


{-| Generates a user-friendly string from settings form field that can be used for a labels or as values
for select/radio. Please note that this cannot be used as form name due to possible spaces.
-}
settingsFieldToString : SettingsFormField -> String
settingsFieldToString field =
    case field of
        PreparationField ->
            "Preparation"

        ClosedField ->
            "Closed"

        CanceledField ->
            "Canceled"

        TimedOutField ->
            "Timed out"

        RejectedField ->
            "Rejected"

        ErrorField ->
            "Error"

        ImmediateTrashField ->
            "Immediately trash"

        SelectedNewSettingField ->
            "Selected new setting"


stringToSettingsField : String -> Maybe SettingsFormField
stringToSettingsField field =
    case field of
        "Preparation" ->
            Just PreparationField

        "Closed" ->
            Just ClosedField

        "Canceled" ->
            Just CanceledField

        "Timed out" ->
            Just TimedOutField

        "Rejected" ->
            Just RejectedField

        "Error" ->
            Just ErrorField

        "Immediately trash" ->
            Just ImmediateTrashField

        "Selected new setting" ->
            Just SelectedNewSettingField

        _ ->
            Nothing


{-| Returns initial fields for user group form and filled in data retention settings related to days.
-}
initialFields : UserGroup -> ( List ( String, Field.Field ), List SettingsFormField )
initialFields data =
    let
        filterDataRetentionDaySettings filter =
            dataRetentionDayFields data.settings
                |> List.filter (\( _, fieldType ) -> filter fieldType)
                |> List.map Tuple.first

        displayedSettings =
            filterDataRetentionDaySettings (\s -> String.isEmpty s |> not)
    in
    ( [ ( groupFieldToName SettingsGroup
        , let
            settingsToFieldGroup =
                List.map (\( fieldType, value ) -> ( settingsFormFieldToName fieldType, Field.string value ))
          in
          List.concat
            [ dataRetentionDayFields data.settings |> settingsToFieldGroup
            , [ ( settingsFormFieldToName ImmediateTrashField, data.settings.isImmediatelyTrashed |> Field.bool )
              , ( settingsFormFieldToName SelectedNewSettingField, filterDataRetentionDaySettings String.isEmpty |> List.head |> Maybe.map settingsFieldToString |> Maybe.withDefault "" |> Field.string )
              ]
            ]
            |> Field.group
        )
      , ( groupFieldToName ContactDetailsGroup
        , Field.group
            [ ( contactDetailsFormFieldToName CompanyNameField, data.contactDetails.name |> Field.string )
            , ( contactDetailsFormFieldToName PhoneField, data.contactDetails.phone |> Field.string )
            , ( contactDetailsFormFieldToName EmailField, data.contactDetails.email |> Field.string )
            , ( contactDetailsFormFieldToName AddressField, data.contactDetails.address |> Field.string )
            , ( contactDetailsFormFieldToName ZipField, data.contactDetails.zip |> Field.string )
            , ( contactDetailsFormFieldToName CityField, data.contactDetails.city |> Field.string )
            , ( contactDetailsFormFieldToName CountryField, data.contactDetails.country |> Field.string )
            , ( contactDetailsFormFieldToName PreferredContactMethodField, data.contactDetails.preferredContactMethod |> contactMethodToString |> Field.string )
            ]
        )
      , ( groupFieldToName TagsGroup
        , List.map
            (\tag ->
                Field.group
                    [ ( tagsFormFieldToName TagNameField, tag.name |> Field.string )
                    , ( tagsFormFieldToName TagValueField, tag.value |> Field.string )
                    ]
            )
            data.tags
            |> Field.list
        )
      ]
    , displayedSettings
    )


{-| Returns the data retention fields related to days of retention together with their values from the provided Data retention policy.
Required to keep one source of truth for which fields correspond to days of retention in User Group form.
-}
dataRetentionDayFields : DataRetentionPolicy -> List ( SettingsFormField, String )
dataRetentionDayFields policy =
    [ ( PreparationField, policy.idleDocTimeoutPreparation )
    , ( ClosedField, policy.idleDocTimeoutClosed )
    , ( CanceledField, policy.idleDocTimeoutCanceled )
    , ( TimedOutField, policy.idleDocTimeoutTimedout )
    , ( RejectedField, policy.idleDocTimeoutRejected )
    , ( ErrorField, policy.idleDocTimeoutError )
    ]
        |> List.map
            (\( fieldType, value ) ->
                ( fieldType, value |> Maybe.map String.fromInt |> Maybe.withDefault "" )
            )



---- FORM VALIDATIONS ----


validateUserGroupForm : Validation String UserGroupFormInput
validateUserGroupForm =
    Validation.map4
        UserGroupFormInput
        (field (groupFieldToName SettingsGroup) validateDataRetentionPolicy)
        (field (groupFieldToName SettingsGroup) (field (settingsFormFieldToName SelectedNewSettingField) validateNewDataRetentionSetting))
        (field (groupFieldToName ContactDetailsGroup) validateContactDetails)
        (field (groupFieldToName TagsGroup) validateTags)


validateDataRetentionPolicy : Validation String DataRetentionPolicy
validateDataRetentionPolicy =
    let
        validateNumberOfRetentionDays =
            oneOf
                [ emptyString |> Validation.map String.toInt
                , Validation.map Just
                    (int
                        |> Validation.andThen (maxInt maxRetentionDays)
                        |> Validation.andThen (minInt minRetentionDays)
                        |> withCustomError
                            ("Please fill in this field with a number between "
                                ++ String.fromInt minRetentionDays
                                ++ " and "
                                ++ String.fromInt maxRetentionDays
                                ++ "."
                            )
                    )
                ]
    in
    Validation.map7
        DataRetentionPolicy
        (field (settingsFormFieldToName PreparationField) validateNumberOfRetentionDays)
        (field (settingsFormFieldToName ClosedField) validateNumberOfRetentionDays)
        (field (settingsFormFieldToName CanceledField) validateNumberOfRetentionDays)
        (field (settingsFormFieldToName TimedOutField) validateNumberOfRetentionDays)
        (field (settingsFormFieldToName RejectedField) validateNumberOfRetentionDays)
        (field (settingsFormFieldToName ErrorField) validateNumberOfRetentionDays)
        (field (settingsFormFieldToName ImmediateTrashField) bool)


validatePhoneNumber : Validation String String
validatePhoneNumber =
    {- This regular expression validates phone according to E.164 standard which allows for 15 digits of phone number,
       and doesn't allow the country code to start with 0.
       A plus prefix option is added due to plus being widely used by users for country code prefix instead of 00.
       The error message kindly points the user into direction of adding a plus followed by numbers only.

       This is a very basic input validation according to E.164, validating that the number contains correct country code
       and number would require a library or extensive code.
    -}
    "^\\+[1-9]\\d{1,14}$"
        |> Regex.fromString
        |> Maybe.map
            (\r ->
                validateStringNotBlank
                    |> Validation.andThen (format r)
                    |> withCustomError "Please fill in the phone number starting with + and following with numbers only."
            )
        |> Maybe.withDefault ("Incorrect phone number validation. Please contact your system administrator." |> customError |> fail)


{-| Validates that the string is not empty or blank. I think this would be a useful function to include in Scrive/elm-form.
-}
validateStringNotBlank : Validation String String
validateStringNotBlank =
    customValidation Validation.string
        (\s ->
            if isBlank s then
                customError "This field cannot be empty or consist only of spaces." |> Err

            else
                Ok s
        )


{-| Validates a field that is a string and that can be empty. I think this would be a useful function to include in Scrive/elm-form,
maybe with a different name (but emptyString and string are already occupied). I think normal "string" validation shouldn't even include
is-not-empty validation, unless your best practice is to always wrap your String in Maybe if it is possible for it to be empty...
-}
validateString : Validation String String
validateString v =
    case Field.asString v of
        Just s ->
            Ok s

        Nothing ->
            Err (Error.value InvalidString)


validateContactDetails : Validation String ContactDetails
validateContactDetails =
    {-
       If post is selected then the address (consisting of address, zip, city & country) is mandatory to be filled.
       If email is selected, then the email is mandatory to be filled.
       If phone is selected then the phone is mandatory to be filled.
    -}
    field (contactDetailsFormFieldToName PreferredContactMethodField) validatePreferredContactMethod
        |> Validation.andThen
            (\preferredContactMethod ->
                let
                    customErrorForPreferredContactMethod contactMethodName =
                        contactMethodName ++ " is selected as preferred contact method. Please fill in this field." |> withCustomError
                in
                case preferredContactMethod of
                    Post ->
                        Validation.map8
                            ContactDetails
                            (field (contactDetailsFormFieldToName CompanyNameField) validateString)
                            (field (contactDetailsFormFieldToName PhoneField) (oneOf [ emptyString, validatePhoneNumber ]))
                            (field (contactDetailsFormFieldToName EmailField) (oneOf [ emptyString, Validation.email ]))
                            (field (contactDetailsFormFieldToName AddressField) (validateStringNotBlank |> customErrorForPreferredContactMethod "Address"))
                            (field (contactDetailsFormFieldToName ZipField) (validateStringNotBlank |> customErrorForPreferredContactMethod "Address"))
                            (field (contactDetailsFormFieldToName CityField) (validateStringNotBlank |> customErrorForPreferredContactMethod "Address"))
                            (field (contactDetailsFormFieldToName CountryField) (validateStringNotBlank |> customErrorForPreferredContactMethod "Address"))
                            (succeed preferredContactMethod)

                    Phone ->
                        Validation.map8
                            ContactDetails
                            (field (contactDetailsFormFieldToName CompanyNameField) validateString)
                            (field (contactDetailsFormFieldToName PhoneField) (validateStringNotBlank |> customErrorForPreferredContactMethod "Phone" |> Validation.andThen (\_ -> validatePhoneNumber)))
                            (field (contactDetailsFormFieldToName EmailField) (oneOf [ emptyString, Validation.email ]))
                            (field (contactDetailsFormFieldToName AddressField) validateString)
                            (field (contactDetailsFormFieldToName ZipField) validateString)
                            (field (contactDetailsFormFieldToName CityField) validateString)
                            (field (contactDetailsFormFieldToName CountryField) validateString)
                            (succeed preferredContactMethod)

                    Email ->
                        Validation.map8
                            ContactDetails
                            (field (contactDetailsFormFieldToName CompanyNameField) validateString)
                            (field (contactDetailsFormFieldToName PhoneField) (oneOf [ emptyString, validatePhoneNumber ]))
                            (field (contactDetailsFormFieldToName EmailField) (validateStringNotBlank |> customErrorForPreferredContactMethod "Email" |> Validation.andThen (\_ -> Validation.email)))
                            (field (contactDetailsFormFieldToName AddressField) validateString)
                            (field (contactDetailsFormFieldToName ZipField) validateString)
                            (field (contactDetailsFormFieldToName CityField) validateString)
                            (field (contactDetailsFormFieldToName CountryField) validateString)
                            (succeed preferredContactMethod)
            )


validatePreferredContactMethod : Validation String ContactMethod
validatePreferredContactMethod =
    customValidation validateStringNotBlank (\s -> Result.fromMaybe (customError "Invalid contact method.") (stringToContactMethod s))


validateNewDataRetentionSetting : Validation String SettingsFormField
validateNewDataRetentionSetting =
    customValidation validateStringNotBlank (\s -> Result.fromMaybe (customError "Invalid data retention setting.") (stringToSettingsField s))


validateTags : Validation String (List Tag)
validateTags =
    list validateTag
        |> Validation.andThen
            (\tags ->
                if tags |> List.map .name |> List.allDifferent then
                    succeed tags

                else
                    customError "Tag names must be unique." |> fail
            )


validateTag : Validation String Tag
validateTag =
    Validation.map2
        Tag
        (field (tagsFormFieldToName TagNameField) (oneOf [ emptyString, validateStringNotBlank |> Validation.andThen (maxLength 32) ]))
        (succeed (tagsFormFieldToName TagValueField))


forErrorToString : ErrorValue String -> String
forErrorToString err =
    case err of
        Empty ->
            "Please fill in this field."

        InvalidEmail ->
            "Please provide a valid email address."

        InvalidInt ->
            "Please fill in a number."

        LongerStringThan i ->
            "This field cannot be longer than " ++ String.fromInt i ++ " characters."

        CustomError e ->
            e

        _ ->
            "Please verify that this field is filled in correctly."


{-| Number of days that is a minimum for retention days.
Declared separately for consistent use in both validation of the form and html attribute limitations.
-}
minRetentionDays : Int
minRetentionDays =
    0


{-| Number of days that is a maximum for retention days.
Declared separately for consistent use in both validation of the form and html attribute limitations.
-}
maxRetentionDays : Int
maxRetentionDays =
    365



---- JSON ----


{-| Helper decoding the nullable string into an empty string if the string is null.
-}
nullableStringDecoder : Decoder String
nullableStringDecoder =
    Decode.string |> Decode.nullable |> Decode.map (Maybe.withDefault "")


inheritanceDecoder : Decoder UserGroupInheritance
inheritanceDecoder =
    Decode.oneOf
        [ Decode.map3 UserGroupInheritanceDetails
            (Decode.field "parent_id" Decode.string)
            (Decode.at [ "settings", "inherited_from" ] nullableStringDecoder)
            (Decode.at [ "contact_details", "inherited_from" ] nullableStringDecoder)
            |> Decode.map Child
        , Decode.succeed Root
        ]


dataRetentionDecoder : Decoder DataRetentionPolicy
dataRetentionDecoder =
    Decode.map7 DataRetentionPolicy
        (Decode.field "idle_doc_timeout_preparation" (Decode.int |> Decode.nullable))
        (Decode.field "idle_doc_timeout_closed" (Decode.int |> Decode.nullable))
        (Decode.field "idle_doc_timeout_canceled" (Decode.int |> Decode.nullable))
        (Decode.field "idle_doc_timeout_timedout" (Decode.int |> Decode.nullable))
        (Decode.field "idle_doc_timeout_rejected" (Decode.int |> Decode.nullable))
        (Decode.field "idle_doc_timeout_error" (Decode.int |> Decode.nullable))
        (Decode.field "immediate_trash" Decode.bool)


contactMethodDecoder : String -> Decoder ContactMethod
contactMethodDecoder contactMethod =
    case contactMethod of
        "email" ->
            Decode.succeed Email

        "phone" ->
            Decode.succeed Phone

        "post" ->
            Decode.succeed Post

        unknownContactMethod ->
            "Unknown contact method " ++ unknownContactMethod ++ " (contact method can only be email, phone or post)" |> Decode.fail


contactDetailsDecoder : Decoder ContactDetails
contactDetailsDecoder =
    Decode.map8 ContactDetails
        (Decode.field "company_name" nullableStringDecoder)
        (Decode.field "phone" nullableStringDecoder)
        (Decode.field "email" nullableStringDecoder)
        (Decode.field "address" nullableStringDecoder)
        (Decode.field "zip" nullableStringDecoder)
        (Decode.field "city" nullableStringDecoder)
        (Decode.field "country" nullableStringDecoder)
        (Decode.field "preferred_contact_method" Decode.string |> Decode.andThen contactMethodDecoder)


userGroupChildDecoder : Decoder UserGroupChild
userGroupChildDecoder =
    Decode.map2 UserGroupChild
        (Decode.field "id" Decode.string)
        (Decode.field "name" Decode.string)


tagDecoder : Decoder Tag
tagDecoder =
    Decode.map2 Tag
        (Decode.field "name" Decode.string)
        (Decode.field "value" Decode.string |> Decode.maybe |> Decode.map (Maybe.withDefault ""))


userGroupDecoder : Decoder UserGroup
userGroupDecoder =
    Decode.map7 UserGroup
        (Decode.field "id" Decode.string)
        inheritanceDecoder
        (Decode.field "name" Decode.string)
        (Decode.field "children" (Decode.list userGroupChildDecoder))
        (Decode.at [ "settings", "data_retention_policy" ] dataRetentionDecoder)
        (Decode.at [ "contact_details", "address" ] contactDetailsDecoder)
        (Decode.field "tags" (Decode.list tagDecoder))



---- General helpers ----


{-| Returns elements of the second list which are not present in the first list.
Removes all occurrences of the element from the second even if it is found less times in the first list.

    listDiff [1, 2, 3][1, 2, 3, 3, 4] = [4]
    listDiff [1, 1, 2, 2, 3, 3][1, 2, 3, 3, 3, 4, 4, 4] = [4, 4, 4]

-}
listDiff : List a -> List a -> List a
listDiff listToSubsctract list =
    List.filter (\el -> List.notMember el listToSubsctract) list
