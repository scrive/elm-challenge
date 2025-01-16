module Modules.Contact exposing (..)

import Components.Input as Input
import Html exposing (Html)
import Html.Attributes as Attrs
import Html.Events as Events
import Json.Decode as Decode
import Regex
import Task


type alias Model =
    { preferredContactMethod : PreferredContactMethod
    , email : String
    , phone : String
    , companyName : String
    , address : String
    , zip : String
    , city : String
    , country : String
    , isInherited : Bool
    , error : ContactFormError
    }


type PreferredContactMethod
    = Email
    | Phone
    | Post


preferredContactMethodDecoder : Decode.Decoder PreferredContactMethod
preferredContactMethodDecoder =
    Decode.string
        |> Decode.andThen
            (\method ->
                case method of
                    "email" ->
                        Decode.succeed Email

                    "phone" ->
                        Decode.succeed Phone

                    "letter" ->
                        Decode.succeed Post

                    _ ->
                        Decode.fail "Not valid contact method"
            )


contactMethodToString : PreferredContactMethod -> String
contactMethodToString method =
    case method of
        Email ->
            "email"

        Phone ->
            "phone"

        Post ->
            "post"


allContactMethods : List PreferredContactMethod
allContactMethods =
    toAllContactMethods Email []


toAllContactMethods : PreferredContactMethod -> List PreferredContactMethod -> List PreferredContactMethod
toAllContactMethods method methods =
    case method of
        Email ->
            toAllContactMethods Phone (Email :: methods)

        Phone ->
            toAllContactMethods Post (Phone :: methods)

        Post ->
            Post :: methods


type alias ContactFormError =
    Maybe ( ContactFormField, String )


type ContactFormField
    = EmailField
    | PhoneField
    | AddressField
    | ZipField
    | CityField
    | CountryField


errorToMessage : ContactFormField -> ( ContactFormField, String ) -> Maybe String
errorToMessage field error =
    if Tuple.first error == field then
        Just (Tuple.second error)

    else
        Nothing


initialModel :
    { preferredContactMethod : PreferredContactMethod
    , email : String
    , phone : String
    , companyName : String
    , address : String
    , zip : String
    , city : String
    , country : String
    }
    -> Bool
    -> Model
initialModel address isInherited =
    { preferredContactMethod = address.preferredContactMethod
    , email = address.email
    , phone = address.phone
    , companyName = address.companyName
    , address = address.address
    , zip = address.zip
    , city = address.city
    , country = address.country
    , isInherited = isInherited
    , error = Nothing
    }


type Msg
    = PreferredContactMethodChanged PreferredContactMethod
    | EmailChanged String
    | PhoneChanged String
    | CompanyNameChanged String
    | AddressChanged String
    | ZipChanged String
    | CityChanged String
    | CountryChanged String
    | Submitted
    | Closed


update : Msg -> Model -> { onSubmit : Model -> msg, onClose : msg } -> ( Model, Cmd msg )
update msg model config =
    case msg of
        PreferredContactMethodChanged method ->
            ( { model | preferredContactMethod = method }
            , Cmd.none
            )

        EmailChanged email ->
            ( { model | email = email }
            , Cmd.none
            )

        PhoneChanged phone ->
            ( { model | phone = phone }
            , Cmd.none
            )

        CompanyNameChanged companyName ->
            ( { model | companyName = companyName }
            , Cmd.none
            )

        AddressChanged address ->
            ( { model | address = address }
            , Cmd.none
            )

        ZipChanged zip ->
            ( { model | zip = zip }
            , Cmd.none
            )

        CityChanged city ->
            ( { model | city = city }
            , Cmd.none
            )

        CountryChanged country ->
            ( { model | country = country }
            , Cmd.none
            )

        Submitted ->
            let
                error : Maybe ( ContactFormField, String )
                error =
                    case model.preferredContactMethod of
                        Email ->
                            emailError model.email

                        Phone ->
                            phoneError model.phone

                        Post ->
                            postError
                                { address = model.address
                                , zip = model.zip
                                , city = model.city
                                , country = model.country
                                }
            in
            case error of
                Nothing ->
                    ( { model | error = Nothing }
                    , Task.perform config.onSubmit (Task.succeed model)
                    )

                Just _ ->
                    ( { model | error = error }
                    , Cmd.none
                    )

        Closed ->
            ( { model | error = Nothing }
            , Task.perform (\_ -> config.onClose) (Task.succeed "")
            )


postError :
    { address : String
    , zip : String
    , city : String
    , country : String
    }
    -> Maybe ( ContactFormField, String )
postError { address, zip, city, country } =
    if String.isEmpty address then
        Just ( AddressField, "address is required" )

    else if String.isEmpty zip then
        Just ( ZipField, "zip is required" )

    else if String.isEmpty city then
        Just ( CityField, "city is required" )

    else if String.isEmpty country then
        Just ( CountryField, "country is required" )

    else
        Nothing


emailError : String -> Maybe ( ContactFormField, String )
emailError email =
    let
        emailRegex : Regex.Regex
        emailRegex =
            Maybe.withDefault
                Regex.never
                (Regex.fromString "^[a-zA-Z0-9._%+-]+@[a-zA-Z0-9.-]+\\.[a-zA-Z]{2,}$")
    in
    if String.isEmpty email then
        Just ( EmailField, "email cannot be empty." )

    else if not (Regex.contains emailRegex email) then
        Just ( EmailField, "invalid e-mail" )

    else
        Nothing


phoneError : String -> Maybe ( ContactFormField, String )
phoneError phone =
    let
        phoneRegex : Regex.Regex
        phoneRegex =
            Maybe.withDefault
                Regex.never
                (Regex.fromString "^\\+?[0-9][0-9\\s]*$")
    in
    if String.isEmpty phone then
        Just ( PhoneField, "phone cannot be empty." )

    else if not (Regex.contains phoneRegex phone) then
        Just ( PhoneField, "invalid phone" )

    else
        Nothing


view : Model -> Html.Html Msg
view model =
    Html.form
        [ Attrs.class "flex flex-col gap-4 my-2 p-2.5 w-full sm:w-auto border rounded whitespace-nowrap text-ellipsis overflow-hidden"
        , Events.onSubmit Submitted
        ]
        [ viewPreferredContactMethods model.isInherited model.preferredContactMethod
        , viewEmail model.isInherited model.email model.error
        , viewPhone model.isInherited model.phone model.error
        , viewCompanyName model.isInherited model.companyName
        , viewAddress model.isInherited model
        , Html.span
            [ Attrs.class "flex flex-row gap-4"
            , Attrs.classList
                [ ( "justify-end", not model.isInherited )
                , ( "justify-center", model.isInherited )
                ]
            ]
            (if model.isInherited then
                [ Html.button
                    [ Attrs.class "border border-black black rounded px-2 py-1 text-black w-2/6 hover:bg-[#d2e7f9]"
                    , Attrs.type_ "button"
                    , Events.onClick Closed
                    ]
                    [ Html.text "close" ]
                ]

             else
                [ Html.button
                    [ Attrs.class "border border-black black rounded px-2 py-1 text-black hover:bg-[#d2e7f9]"
                    , Attrs.type_ "button"
                    , Events.onClick Closed
                    ]
                    [ Html.text "cancel" ]
                , Html.button
                    [ Attrs.class "border border-transparent rounded px-2 py-1 bg-[#1e88e2] text-white outline-black hover:text-[#d2e7f9]"
                    , Attrs.type_ "submit"
                    , Events.onClick Submitted
                    ]
                    [ Html.text "apply" ]
                ]
            )
        ]


viewPreferredContactMethods : Bool -> PreferredContactMethod -> Html Msg
viewPreferredContactMethods isInherited preferredContactMethod =
    Html.span
        [ Attrs.class "rounded"
        , Attrs.classList [ ( "bg-[#e8f3fc]", isInherited ) ]
        ]
        [ Html.label [ Attrs.class "text-sm pl-2" ]
            [ Html.text "Preferred contact method" ]
        , Html.div
            [ Attrs.class "flex gap-2 p-2"
            , Attrs.classList [ ( "border-transparent", isInherited ) ]
            ]
            (allContactMethods
                |> List.map
                    (\method ->
                        Html.button
                            [ Attrs.class "border rounded px-2 py-1 w-2/6 sm:w-1/6"
                            , Attrs.classList
                                [ ( "bg-[#4ba0e8] border-transparent text-white", method == preferredContactMethod )
                                , ( "hover:bg-[#d2e7f9]", method /= preferredContactMethod && not isInherited )
                                , ( "border-transparent", isInherited )
                                ]
                            , Attrs.disabled isInherited
                            , Events.onClick (PreferredContactMethodChanged method)
                            , Attrs.type_ "button"
                            ]
                            [ Html.text (contactMethodToString method) ]
                    )
            )
        ]


viewEmail : Bool -> String -> ContactFormError -> Html Msg
viewEmail isInherited email error =
    Input.viewTextOrNumber
        { label = "e-mail"
        , disabled = isInherited
        , type_ = "email"
        , onChange = EmailChanged
        , value = email
        , errorMessage = error |> Maybe.andThen (errorToMessage EmailField)
        }


viewPhone : Bool -> String -> ContactFormError -> Html Msg
viewPhone isInherited phone error =
    Input.viewTextOrNumber
        { label = "phone"
        , disabled = isInherited
        , type_ = "tel"
        , onChange = PhoneChanged
        , value = phone
        , errorMessage = error |> Maybe.andThen (errorToMessage PhoneField)
        }


viewCompanyName : Bool -> String -> Html Msg
viewCompanyName isInherited companyName =
    Input.viewTextOrNumber
        { label = "company name"
        , disabled = isInherited
        , type_ = "text"
        , onChange = CompanyNameChanged
        , value = companyName
        , errorMessage = Nothing
        }


viewAddress : Bool -> Model -> Html Msg
viewAddress isInherited { address, zip, city, country, error } =
    Html.div
        [ Attrs.class "flex flex-col rounded py-1"
        , Attrs.classList [ ( "bg-[#e8f3fc]", isInherited ) ]
        ]
        [ Html.span [ Attrs.class "flex flex-col sm:flex-row  w-full" ]
            [ Html.span [ Attrs.class "w-full sm:w-4/6" ]
                [ Input.viewTextOrNumber
                    { label = "address"
                    , disabled = isInherited
                    , type_ = "text"
                    , onChange = AddressChanged
                    , value = address
                    , errorMessage = error |> Maybe.andThen (errorToMessage AddressField)
                    }
                ]
            , Html.span [ Attrs.class "w-full sm:w-2/6" ]
                [ Input.viewTextOrNumber
                    { label = "zip"
                    , disabled = isInherited
                    , type_ = "text"
                    , onChange = ZipChanged
                    , value = zip
                    , errorMessage = error |> Maybe.andThen (errorToMessage ZipField)
                    }
                ]
            ]
        , Html.span [ Attrs.class "flex flex-col sm:flex-row w-full sm:w-auto" ]
            [ Html.span [ Attrs.class "w-full sm:w-3/6" ]
                [ Input.viewTextOrNumber
                    { label = "city"
                    , disabled = isInherited
                    , type_ = "text"
                    , onChange = CityChanged
                    , value = city
                    , errorMessage = error |> Maybe.andThen (errorToMessage CityField)
                    }
                ]
            , Html.span [ Attrs.class "w-full sm:w-3/6" ]
                [ Input.viewTextOrNumber
                    { label = "country"
                    , disabled = isInherited
                    , type_ = "text"
                    , onChange = CountryChanged
                    , value = country
                    , errorMessage = error |> Maybe.andThen (errorToMessage CountryField)
                    }
                ]
            ]
        ]
