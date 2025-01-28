module Component.ContactDetails exposing (..)

import Component.DataRetentionSettings exposing (isFieldChanged)
import Data.UserGroup
    exposing
        ( Address
        , PreferredContactMethod(..)
        )
import Debounce
import Email
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onBlur, onClick, onInput)
import Maybe.Extra as Maybe
import Regex
import Svg
import Svg.Attributes as SvgAttrs
import Task


type alias Model =
    { isInherited : Bool
    , currentContactMethod : PreferredContactMethod
    , selectedContactMethod : PreferredContactMethod
    , email : ContactField
    , phone : ContactField
    , companyName : ContactField
    , address : ContactField
    , zip : ContactField
    , city : ContactField
    , country : ContactField

    -- Debouncers
    , emailDebouncer : Debounce.Debounce String
    , phoneDebouncer : Debounce.Debounce String
    }


init : Bool -> Address -> Model
init isInherited contactDetails =
    let
        toContactField field =
            { initialValue = Maybe.withDefault "" field
            , currentValue = Maybe.withDefault "" field
            , isValidated = False
            }
    in
    { isInherited = isInherited
    , currentContactMethod = contactDetails.preferredContactMethod
    , selectedContactMethod = contactDetails.preferredContactMethod
    , email = toContactField contactDetails.email
    , phone = toContactField contactDetails.phone
    , companyName = toContactField contactDetails.companyName
    , address = toContactField contactDetails.address
    , zip = toContactField contactDetails.zip
    , city = toContactField contactDetails.city
    , country = toContactField contactDetails.country
    , emailDebouncer = Debounce.init
    , phoneDebouncer = Debounce.init
    }


toContactDetailsField : String -> Maybe String
toContactDetailsField field =
    if String.isEmpty field then
        Nothing

    else
        Just field


toContactDetailsAddress : Model -> Address
toContactDetailsAddress model =
    { preferredContactMethod = model.selectedContactMethod
    , email = toContactDetailsField model.email.currentValue
    , phone = toContactDetailsField model.phone.currentValue
    , companyName = toContactDetailsField model.companyName.currentValue
    , address = toContactDetailsField model.address.currentValue
    , zip = toContactDetailsField model.zip.currentValue
    , city = toContactDetailsField model.city.currentValue
    , country = toContactDetailsField model.country.currentValue
    }


type alias ContactField =
    { initialValue : String
    , currentValue : String
    , isValidated : Bool
    }


updateCurrentValue : String -> ContactField -> ContactField
updateCurrentValue value field =
    { field | currentValue = value }


updateIsValidated : Bool -> ContactField -> ContactField
updateIsValidated isValidated field =
    { field | isValidated = isValidated }



-- Debouncer --


debounceConfig : (Debounce.Msg -> Msg) -> Debounce.Config Msg
debounceConfig debounceMsg =
    { strategy = Debounce.later 500
    , transform = debounceMsg
    }


save : (String -> Msg) -> String -> Cmd Msg
save toMsg s =
    Task.perform toMsg (Task.succeed s)



-- Update --


type Msg
    = NoOp
    | SelectedContactMethodChanged PreferredContactMethod
    | EmailFieldOnBlur
    | PhoneFieldOnBlur
    | EmailChanged String
    | DebounceEmail Debounce.Msg
    | EmailSettled String
    | PhoneChanged String
    | DebouncePhone Debounce.Msg
    | PhoneSettled String
    | CompanyNameChanged String
    | AddressChanged String
    | ZipChanged String
    | CityChanged String
    | CountryChanged String
    | ApplyButtonClicked


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SelectedContactMethodChanged contactMethod ->
            ( { model | selectedContactMethod = contactMethod }, Cmd.none )

        EmailFieldOnBlur ->
            ( { model | email = updateIsValidated True model.email }, Cmd.none )

        PhoneFieldOnBlur ->
            ( { model | phone = updateIsValidated True model.phone }, Cmd.none )

        EmailChanged value ->
            let
                ( newDebouncer, cmd ) =
                    Debounce.push
                        (debounceConfig DebounceEmail)
                        value
                        model.emailDebouncer
            in
            ( { model
                | email = updateCurrentValue value model.email
                , emailDebouncer = newDebouncer
              }
            , cmd
            )

        DebounceEmail msg_ ->
            let
                ( newDebouncer, cmd ) =
                    Debounce.update
                        (debounceConfig DebounceEmail)
                        (Debounce.takeLast (save EmailSettled))
                        msg_
                        model.emailDebouncer
            in
            ( { model | emailDebouncer = newDebouncer }
            , cmd
            )

        EmailSettled s ->
            let
                isValid =
                    emailValidationError s
                        |> Maybe.isNothing
            in
            ( { model | email = updateIsValidated (not isValid) model.email }, Cmd.none )

        PhoneChanged value ->
            let
                ( newDebouncer, cmd ) =
                    Debounce.push
                        (debounceConfig DebouncePhone)
                        value
                        model.phoneDebouncer
            in
            ( { model
                | phone = updateCurrentValue value model.phone
                , phoneDebouncer = newDebouncer
              }
            , cmd
            )

        DebouncePhone msg_ ->
            let
                ( newDebouncer, cmd ) =
                    Debounce.update
                        (debounceConfig DebouncePhone)
                        (Debounce.takeLast (save PhoneSettled))
                        msg_
                        model.phoneDebouncer
            in
            ( { model | phoneDebouncer = newDebouncer }
            , cmd
            )

        PhoneSettled s ->
            let
                isValid =
                    phoneValidationError s
                        |> Maybe.isNothing
            in
            ( { model | phone = updateIsValidated (not isValid) model.phone }, Cmd.none )

        CompanyNameChanged value ->
            ( { model | companyName = updateCurrentValue value model.companyName }, Cmd.none )

        AddressChanged value ->
            ( { model | address = updateCurrentValue value model.address }, Cmd.none )

        ZipChanged value ->
            ( { model | zip = updateCurrentValue value model.zip }, Cmd.none )

        CityChanged value ->
            ( { model | city = updateCurrentValue value model.city }, Cmd.none )

        CountryChanged value ->
            ( { model | country = updateCurrentValue value model.country }, Cmd.none )

        _ ->
            ( model, Cmd.none )



-- Validation --


isFormChanged : Model -> Bool
isFormChanged model =
    let
        isFieldChanged field =
            field.initialValue /= field.currentValue
    in
    List.any isFieldChanged
        [ model.email
        , model.phone
        , model.companyName
        , model.address
        , model.zip
        , model.city
        , model.country
        ]
        || (model.selectedContactMethod /= model.currentContactMethod)


emailValidationError : String -> Maybe String
emailValidationError emailField =
    if Email.isValid emailField || String.isEmpty emailField then
        Nothing

    else
        Just "Invalid e-mail address"


phoneValidationError : String -> Maybe String
phoneValidationError phoneField =
    let
        phoneRegex =
            Regex.fromString "^\\+((?:9[679]|8[035789]|6[789]|5[90]|42|3[578]|2[1-689])|9[0-58]|8[1246]|6[0-6]|5[1-8]|4[013-9]|3[0-469]|2[70]|7|1)(?:\\W*\\d){0,13}\\d$"
                |> Maybe.withDefault Regex.never
    in
    if Regex.contains phoneRegex phoneField || String.isEmpty phoneField then
        Nothing

    else
        Just "Invalid phone number"


isPreferredContactValid : Model -> Bool
isPreferredContactValid model =
    case model.selectedContactMethod of
        Phone ->
            not <| String.isEmpty model.phone.currentValue

        Email ->
            not <| String.isEmpty model.email.currentValue

        Post ->
            not <| String.isEmpty model.address.currentValue


isFormValid : Model -> Bool
isFormValid model =
    List.all Maybe.isNothing
        [ emailValidationError model.email.currentValue
        , phoneValidationError model.phone.currentValue
        ]
        && isPreferredContactValid model



-- View --


contactDetailTextForm : Bool -> String -> ContactField -> (String -> Maybe String) -> Bool -> Msg -> (String -> Msg) -> List (Html Msg)
contactDetailTextForm isInherited id_ field isValid isWarning toBlurMsg toMsg =
    let
        validationString =
            if field.isValidated then
                isValid field.currentValue
                    |> Maybe.map (\s -> p [ class "mt-1 text-red-500 text-xs italic" ] [ text s ])

            else
                Nothing

        warningString =
            if isWarning then
                Just <| p [ class "mt-1 text-yellow-500 text-xs italic" ] [ text "Preferred contact method can not be empty" ]

            else
                Nothing
    in
    [ label [ for id_, class "block text-gray-700 text-sm font-bold mb-2" ] [ text id_ ]
    , if isInherited then
        span [ class "mt-2" ]
            [ text <|
                case field.currentValue of
                    "" ->
                        "N/A"

                    value ->
                        value
            ]

      else
        div [ class "mt-2" ]
            [ input
                [ type_ "text"
                , name id_
                , id id_
                , value field.currentValue
                , classList [ ( "border-red-500", Maybe.isJust validationString ) ]
                , classList [ ( "border-yellow-500", Maybe.isJust warningString ) ]
                , onInput toMsg
                , onBlur toBlurMsg
                , autocomplete True
                , class "shadow appearance-none border rounded w-full py-2 px-3 text-gray-700 leading-tight focus:outline-none focus:shadow-outline"
                ]
                []
            , Maybe.withDefault (text "") validationString
            , Maybe.withDefault (text "") warningString
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
            , onClick ApplyButtonClicked
            ]
            [ text "Apply" ]


toContactMethod : String -> Maybe PreferredContactMethod
toContactMethod contactMethod =
    case contactMethod of
        "Email" ->
            Just Email

        "Post" ->
            Just Post

        "Phone" ->
            Just Phone

        _ ->
            Nothing


fromContactMethod : PreferredContactMethod -> String
fromContactMethod contactMethod =
    case contactMethod of
        Email ->
            "E-mail"

        Post ->
            "Post"

        Phone ->
            "Phone"


preferredContactMethodSelect : Model -> List (Html Msg)
preferredContactMethodSelect model =
    if model.isInherited then
        [ span [] [ text <| fromContactMethod model.selectedContactMethod ] ]

    else
        [ select
            [ name "contact"
            , id "contact"
            , autocomplete True
            , onInput <| toContactMethod >> Maybe.withDefault model.selectedContactMethod >> SelectedContactMethodChanged
            , class "col-start-1 row-start-1 w-full appearance-none rounded-md bg-white py-1.5 pl-3 pr-8 text-base text-gray-900 outline outline-1 -outline-offset-1 outline-gray-300 focus:outline focus:outline-2 focus:-outline-offset-2 focus:outline-indigo-600 sm:text-sm/6"
            ]
            [ option [ value "Email" ] [ text "E-mail" ]
            , option [ value "Post" ] [ text "Post" ]
            , option [ value "Phone" ] [ text "Phone" ]
            ]
        , div [ class "pointer-events-none absolute inset-y-0 right-0 flex items-center px-2 text-gray-700" ]
            [ Svg.svg
                [ SvgAttrs.class "fill-current h-4 w-4"
                , SvgAttrs.viewBox "0 0 20 20"
                ]
                [ Svg.path [ SvgAttrs.d "M9.293 12.95l.707.707L15.657 8l-1.414-1.414L10 10.828 5.757 6.586 4.343 8z" ] [] ]
            ]
        ]


introductionText : String
introductionText =
    """This form contains contact details for the user group. Select a preferred way of contact and make sure that
    the corresponding information is filled in. """


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
    let
        isInherited =
            model.isInherited
    in
    div []
        [ p [ class "mb-8 text-sm" ] <| text introductionText :: inheritedText model.isInherited
        , div [ class "mt-10 grid grid-cols-1 gap-x-6 sm:grid-cols-6" ]
            [ div [ class "sm:col-span-3 h-0 mb-24" ]
                [ label [ for "contact", class "block text-gray-700 text-sm font-bold mb-2" ] [ text "Preferred contact method" ]
                , div [ class "mt-2 relative" ] <| preferredContactMethodSelect model
                ]
            , div [ class "col-span-full h-0 mb-24" ] <|
                contactDetailTextForm
                    isInherited
                    "Street address"
                    model.address
                    (always <| Nothing)
                    (model.selectedContactMethod == Post && String.isEmpty model.address.currentValue)
                    NoOp
                    AddressChanged
            , div [ class "sm:col-span-2 h-0 mb-24" ] <|
                contactDetailTextForm
                    isInherited
                    "E-mail"
                    model.email
                    emailValidationError
                    (model.selectedContactMethod == Email && String.isEmpty model.email.currentValue)
                    EmailFieldOnBlur
                    EmailChanged
            , div [ class "sm:col-span-2 h-0 mb-24" ] <|
                contactDetailTextForm
                    isInherited
                    "Company name"
                    model.companyName
                    (always <| Nothing)
                    False
                    NoOp
                    CompanyNameChanged
            , div [ class "sm:col-span-2 h-0 mb-24" ] <|
                contactDetailTextForm
                    isInherited
                    "Phone number"
                    model.phone
                    phoneValidationError
                    (model.selectedContactMethod == Phone && String.isEmpty model.phone.currentValue)
                    PhoneFieldOnBlur
                    PhoneChanged
            , div [ class "sm:col-span-2 h-0 mb-24" ] <|
                contactDetailTextForm
                    isInherited
                    "Zip code"
                    model.zip
                    (always <| Nothing)
                    False
                    NoOp
                    ZipChanged
            , div [ class "sm:col-span-2 h-0 mb-24" ] <|
                contactDetailTextForm
                    isInherited
                    "City"
                    model.city
                    (always <| Nothing)
                    False
                    NoOp
                    CityChanged
            , div [ class "sm:col-span-2 h-0 mb-24" ] <|
                contactDetailTextForm
                    isInherited
                    "Country"
                    model.country
                    (always <| Nothing)
                    False
                    NoOp
                    CountryChanged
            ]
        , applyButton model
        ]
