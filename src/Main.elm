module Main exposing (main, preferredContactMethodDecoder)

import Browser
import Data
import Html exposing (Html)
import Html.Attributes as Attrs
import Html.Events as Events
import Json.Decode as Decode
import Json.Decode.Pipeline as Decode
import Regex



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



---- MODEL ----


type alias Model =
    { userGroup : UserGroup
    , contactFormError : ContactFormError
    }


type alias ContactFormError =
    Maybe ( ContactFormField, String )


{-| Contact form field.
-}
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



-- User Group


type alias UserGroup =
    { id : String
    , parentId : String
    , name : String
    , children : List Children
    , settings : Settings
    , contactDetails : ContactDetails
    , tags : List Tag
    }


emptyUserGroup : UserGroup
emptyUserGroup =
    { id = ""
    , parentId = ""
    , name = ""
    , children = []
    , settings = emptySettings
    , contactDetails = emptyContactDetails
    , tags = []
    }


userGroupDecoder : Decode.Decoder UserGroup
userGroupDecoder =
    Decode.succeed UserGroup
        |> Decode.required "id" Decode.string
        |> Decode.required "parent_id" Decode.string
        |> Decode.required "name" Decode.string
        |> Decode.required "children" (Decode.list childrenDecoder)
        |> Decode.required "settings" settingsDecoder
        |> Decode.required "contact_details" contactDetailsDecoder
        |> Decode.required "tags" (Decode.list tagDecoder)


type alias Children =
    { id : String
    , name : String
    }


childrenDecoder : Decode.Decoder Children
childrenDecoder =
    Decode.succeed Children
        |> Decode.required "id" Decode.string
        |> Decode.required "name" Decode.string


type alias Tag =
    { name : String
    , value : String
    }


tagDecoder : Decode.Decoder Tag
tagDecoder =
    Decode.succeed Tag
        |> Decode.required "name" Decode.string
        |> Decode.optional "value" Decode.string ""



-- Settings


type alias Settings =
    { inheritedFrom : String
    , dataRetentionPolicy : DataRetentionPolicy
    }


emptySettings : Settings
emptySettings =
    { inheritedFrom = ""
    , dataRetentionPolicy = emptyDataRetentionPolicy
    }


settingsDecoder : Decode.Decoder Settings
settingsDecoder =
    Decode.succeed Settings
        |> Decode.optional "inherited_from" Decode.string ""
        |> Decode.required "data_retention_policy" dataRetentionPolicyDecoder


type alias DataRetentionPolicy =
    { idleDocTimeOutPreparation : Maybe Int
    , idleDocTimeOutClosed : Maybe Int
    , idleDocTimeOutCancelled : Maybe Int
    , idleDocTimeOutTimedOut : Maybe Int
    , idleDocTimeOutRejected : Maybe Int
    , idleDocTimeOutError : Maybe Int
    , immediateTrash : Bool
    }


emptyDataRetentionPolicy : DataRetentionPolicy
emptyDataRetentionPolicy =
    { idleDocTimeOutPreparation = Nothing
    , idleDocTimeOutClosed = Nothing
    , idleDocTimeOutCancelled = Nothing
    , idleDocTimeOutTimedOut = Nothing
    , idleDocTimeOutRejected = Nothing
    , idleDocTimeOutError = Nothing
    , immediateTrash = False
    }


dataRetentionPolicyDecoder : Decode.Decoder DataRetentionPolicy
dataRetentionPolicyDecoder =
    Decode.succeed DataRetentionPolicy
        |> Decode.optional "idle_doc_timeout_preparation" (Decode.map Just Decode.int) Nothing
        |> Decode.optional "idle_doc_timeout_closed" (Decode.map Just Decode.int) Nothing
        |> Decode.optional "idle_doc_timeout_canceled" (Decode.map Just Decode.int) Nothing
        |> Decode.optional "idle_doc_timeout_timedout" (Decode.map Just Decode.int) Nothing
        |> Decode.optional "idle_doc_timeout_rejected" (Decode.map Just Decode.int) Nothing
        |> Decode.optional "idle_doc_timeout_error" (Decode.map Just Decode.int) Nothing
        |> Decode.required "immediate_trash" Decode.bool


type alias ContactDetails =
    { inheritedFrom : String
    , address : Address
    }


emptyContactDetails : ContactDetails
emptyContactDetails =
    { inheritedFrom = ""
    , address = emptyAddress
    }


contactDetailsDecoder : Decode.Decoder ContactDetails
contactDetailsDecoder =
    Decode.succeed ContactDetails
        |> Decode.optional "inherited_from" Decode.string ""
        |> Decode.required "address" addressDecoder


type alias Address =
    { preferredContactMethod : PreferredContactMethod
    , email : String
    , phone : String
    , companyName : String
    , address : String
    , zip : String
    , city : String
    , country : String
    }


emptyAddress : Address
emptyAddress =
    { preferredContactMethod = Email
    , email = ""
    , phone = ""
    , companyName = ""
    , address = ""
    , zip = ""
    , city = ""
    , country = ""
    }


addressDecoder : Decode.Decoder Address
addressDecoder =
    Decode.succeed Address
        |> Decode.required "preferred_contact_method" preferredContactMethodDecoder
        |> Decode.optional "email" Decode.string ""
        |> Decode.optional "phone" Decode.string ""
        |> Decode.optional "company_name" Decode.string ""
        |> Decode.optional "address" Decode.string ""
        |> Decode.optional "zip" Decode.string ""
        |> Decode.optional "city" Decode.string ""
        |> Decode.optional "country" Decode.string ""


type PreferredContactMethod
    = Email
    | Phone
    | Post


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


init : ( Model, Cmd Msg )
init =
    let
        userGroup =
            Decode.decodeString userGroupDecoder Data.userGroup
                |> Result.toMaybe
                |> Maybe.withDefault emptyUserGroup
    in
    ( { userGroup = userGroup, contactFormError = Nothing }
    , Cmd.none
    )



---- UPDATE ----


type Msg
    = NoOp
    | PreferredContactMethodChanged PreferredContactMethod
    | EmailChanged String
    | PhoneChanged String
    | CompanyNameChanged String
    | AddressChanged String
    | ZipChanged String
    | CityChanged String
    | CountryChanged String
    | Submitted


update : Msg -> Model -> ( Model, Cmd Msg )
update msg ({ userGroup } as model) =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        PreferredContactMethodChanged method ->
            let
                toUpdatedContactMethod : ContactDetails -> ContactDetails
                toUpdatedContactMethod ({ address } as contactDetails) =
                    { contactDetails | address = { address | preferredContactMethod = method } }
            in
            ( { model
                | userGroup =
                    { userGroup | contactDetails = toUpdatedContactMethod userGroup.contactDetails }
              }
            , Cmd.none
            )

        EmailChanged email ->
            let
                toUpdatedEmail : ContactDetails -> ContactDetails
                toUpdatedEmail ({ address } as contactDetails) =
                    { contactDetails | address = { address | email = email } }
            in
            ( { model
                | userGroup =
                    { userGroup | contactDetails = toUpdatedEmail userGroup.contactDetails }
              }
            , Cmd.none
            )

        PhoneChanged phone ->
            let
                toUpdatedPhone : ContactDetails -> ContactDetails
                toUpdatedPhone ({ address } as contactDetails) =
                    { contactDetails | address = { address | phone = phone } }
            in
            ( { model
                | userGroup =
                    { userGroup | contactDetails = toUpdatedPhone userGroup.contactDetails }
              }
            , Cmd.none
            )

        CompanyNameChanged companyName ->
            let
                toUpdatedCompanyName : ContactDetails -> ContactDetails
                toUpdatedCompanyName ({ address } as contactDetails) =
                    { contactDetails | address = { address | companyName = companyName } }
            in
            ( { model
                | userGroup =
                    { userGroup | contactDetails = toUpdatedCompanyName userGroup.contactDetails }
              }
            , Cmd.none
            )

        AddressChanged address_ ->
            let
                toUpdatedAddress : ContactDetails -> ContactDetails
                toUpdatedAddress ({ address } as contactDetails) =
                    { contactDetails | address = { address | address = address_ } }
            in
            ( { model
                | userGroup =
                    { userGroup | contactDetails = toUpdatedAddress userGroup.contactDetails }
              }
            , Cmd.none
            )

        ZipChanged zip ->
            let
                toUpdatedZip : ContactDetails -> ContactDetails
                toUpdatedZip ({ address } as contactDetails) =
                    { contactDetails | address = { address | zip = zip } }
            in
            ( { model
                | userGroup =
                    { userGroup | contactDetails = toUpdatedZip userGroup.contactDetails }
              }
            , Cmd.none
            )

        CityChanged city ->
            let
                toUpdatedCity : ContactDetails -> ContactDetails
                toUpdatedCity ({ address } as contactDetails) =
                    { contactDetails | address = { address | address = city } }
            in
            ( { model
                | userGroup =
                    { userGroup | contactDetails = toUpdatedCity userGroup.contactDetails }
              }
            , Cmd.none
            )

        CountryChanged country ->
            let
                toUpdatedCountry : ContactDetails -> ContactDetails
                toUpdatedCountry ({ address } as contactDetails) =
                    { contactDetails | address = { address | address = country } }
            in
            ( { model
                | userGroup =
                    { userGroup | contactDetails = toUpdatedCountry userGroup.contactDetails }
              }
            , Cmd.none
            )

        Submitted ->
            let
                { preferredContactMethod, email, phone } =
                    userGroup.contactDetails.address

                error =
                    case preferredContactMethod of
                        Email ->
                            emailError email

                        Phone ->
                            phoneError phone

                        Post ->
                            Nothing
            in
            ( { model
                | contactFormError = error
              }
            , Cmd.none
            )


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
        Just ( EmailField, "Email cannot be empty." )

    else if not (Regex.contains emailRegex email) then
        Just ( EmailField, "Invalid e-mail" )

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
        Just ( PhoneField, "Phone cannot be empty." )

    else if not (Regex.contains phoneRegex phone) then
        Just ( PhoneField, "Invalid phone" )

    else
        Nothing



---- VIEW ----


view : Model -> Html Msg
view { userGroup, contactFormError } =
    Html.div [ Attrs.class "flex flex-col items-center font-montserrat" ]
        [ viewContact userGroup.contactDetails contactFormError
        ]


viewContact : ContactDetails -> ContactFormError -> Html Msg
viewContact { inheritedFrom, address } error =
    let
        isInherited : Bool
        isInherited =
            not (String.isEmpty inheritedFrom)
    in
    Html.form
        [ Attrs.class "flex flex-col gap-4 my-2 p-2.5 w-auto border rounded"
        , Events.onSubmit Submitted
        ]
        [ viewPreferredContactMethods isInherited address.preferredContactMethod
        , viewEmail isInherited address.email error
        , viewPhone isInherited address.phone error
        , viewCompanyName isInherited address.companyName
        , viewAddress isInherited address
        , Html.span
            [ Attrs.class "flex flex-row gap-4"
            , Attrs.classList
                [ ( "justify-end", not isInherited )
                , ( "justify-center", isInherited )
                ]
            ]
            (if isInherited then
                [ Html.button
                    [ Attrs.class "border border-black black rounded px-2 py-1 text-black w-2/6 hover:bg-[#d2e7f9]"
                    , Events.onClick NoOp
                    ]
                    [ Html.text "close" ]
                ]

             else
                [ Html.button
                    [ Attrs.class "border border-black black rounded px-2 py-1 text-black hover:bg-[#d2e7f9]"
                    , Events.onClick NoOp
                    ]
                    [ Html.text "cancel" ]
                , Html.button
                    [ Attrs.class "border border-transparent rounded px-2 py-1 bg-[#1e88e2] text-white hover:text-[#d2e7f9]"
                    , Events.onClick Submitted
                    ]
                    [ Html.text "apply" ]
                ]
            )
        ]


viewPreferredContactMethods : Bool -> PreferredContactMethod -> Html Msg
viewPreferredContactMethods isInherited preferredContactMethod =
    Html.ul
        [ Attrs.class "flex gap-2 p-2 rounded"
        , Attrs.classList [ ( "bg-[#e8f3fc]", isInherited ) ]
        ]
        (allContactMethods
            |> List.map
                (\method ->
                    Html.button
                        [ Attrs.class "border rounded px-2 py-1"
                        , Attrs.classList
                            [ ( "bg-[#4ba0e8] border-transparent text-white", method == preferredContactMethod )
                            , ( "hover:bg-[#d2e7f9]", method /= preferredContactMethod && not isInherited )
                            , ( "border-transparent", isInherited )
                            ]
                        , Attrs.disabled isInherited
                        , Events.onClick (PreferredContactMethodChanged method)
                        ]
                        [ Html.text (contactMethodToString method) ]
                )
        )


viewEmail : Bool -> String -> ContactFormError -> Html Msg
viewEmail isInherited email error =
    viewInput
        { label = "e-mail"
        , disabled = isInherited
        , type_ = "email"
        , onChange = EmailChanged
        , value = email
        , errorMessage = error |> Maybe.andThen (errorToMessage EmailField)
        }


viewPhone : Bool -> String -> ContactFormError -> Html Msg
viewPhone isInherited phone error =
    viewInput
        { label = "phone"
        , disabled = isInherited
        , type_ = "tel"
        , onChange = PhoneChanged
        , value = phone
        , errorMessage = error |> Maybe.andThen (errorToMessage PhoneField)
        }


viewInput :
    { label : String
    , disabled : Bool
    , type_ : String
    , onChange : String -> Msg
    , value : String
    , errorMessage : Maybe String
    }
    -> Html Msg
viewInput { label, disabled, type_, onChange, value, errorMessage } =
    let
        hasError : Bool
        hasError =
            errorMessage /= Nothing
    in
    Html.span
        [ Attrs.class "flex flex-col rounded px-2 py-1"
        , Attrs.classList [ ( "bg-[#e8f3fc]", disabled ) ]
        ]
        [ Html.label [ Attrs.class "text-sm pl-1", Attrs.classList [ ( "text-red-500", hasError ) ] ]
            [ Html.text (errorMessage |> Maybe.withDefault label) ]
        , Html.input
            [ Attrs.type_ type_
            , Attrs.class "border rounded px-2 py-1 focus:outline-none border-stone-400"
            , Attrs.classList [ ( "bg-[#e8f3fc]", disabled ), ( "border-red-500", hasError ) ]
            , Attrs.disabled disabled
            , Attrs.value value
            , Events.onInput onChange
            ]
            []
        ]


viewCompanyName : Bool -> String -> Html Msg
viewCompanyName isInherited companyName =
    viewInput
        { label = "company name"
        , disabled = isInherited
        , type_ = "text"
        , onChange = CompanyNameChanged
        , value = companyName
        , errorMessage = Nothing
        }


viewAddress : Bool -> Address -> Html Msg
viewAddress isInherited { address, zip, city, country } =
    Html.div
        [ Attrs.class "flex flex-col rounded py-1"
        , Attrs.classList [ ( "bg-[#e8f3fc]", isInherited ) ]
        ]
        [ Html.span [ Attrs.class "flex flex-row w-full" ]
            [ Html.span [ Attrs.class "w-full" ]
                [ viewInput
                    { label = "address"
                    , disabled = isInherited
                    , type_ = "text"
                    , onChange = AddressChanged
                    , value = address
                    , errorMessage = Nothing
                    }
                ]
            , Html.span [ Attrs.class "w-2/6" ]
                [ viewInput
                    { label = "zip"
                    , disabled = isInherited
                    , type_ = "text"
                    , onChange = ZipChanged
                    , value = zip
                    , errorMessage = Nothing
                    }
                ]
            ]
        , Html.span [ Attrs.class "flex flex-row w-full" ]
            [ Html.span [ Attrs.class "w-full" ]
                [ viewInput
                    { label = "city"
                    , disabled = isInherited
                    , type_ = "text"
                    , onChange = CityChanged
                    , value = city
                    , errorMessage = Nothing
                    }
                ]
            , Html.span [ Attrs.class "w-full" ]
                [ viewInput
                    { label = "country"
                    , disabled = isInherited
                    , type_ = "text"
                    , onChange = CountryChanged
                    , value = country
                    , errorMessage = Nothing
                    }
                ]
            ]
        ]
