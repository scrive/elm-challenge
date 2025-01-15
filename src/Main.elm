module Main exposing (main, preferredContactMethodDecoder)

import Browser
import Data
import Html exposing (Html)
import Html.Attributes as Attrs
import Html.Events as Events
import Json.Decode as Decode
import Json.Decode.Pipeline as Decode



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
    { userGroup : UserGroup }


init : ( Model, Cmd Msg )
init =
    let
        userGroup =
            Decode.decodeString userGroupDecoder Data.userGroup
                |> Result.toMaybe
                |> Maybe.withDefault emptyUserGroup
    in
    ( { userGroup = userGroup }
    , Cmd.none
    )



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



---- UPDATE ----


type Msg
    = NoOp
    | PreferredContactMethodChanged PreferredContactMethod
    | EmailChanged String
    | PhoneChanged String


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



---- VIEW ----


view : Model -> Html Msg
view { userGroup } =
    Html.div [ Attrs.class "flex flex-col items-center font-montserrat" ]
        [ viewContact userGroup.contactDetails
        ]


viewContact : ContactDetails -> Html Msg
viewContact { inheritedFrom, address } =
    let
        isInherited : Bool
        isInherited =
            not (String.isEmpty inheritedFrom)
    in
    Html.div [ Attrs.class "flex flex-col gap-4 my-2" ]
        [ viewPreferredContactMethods isInherited address.preferredContactMethod
        , viewEmail isInherited address.email
        , viewPhone isInherited address.phone
        ]


viewPreferredContactMethods : Bool -> PreferredContactMethod -> Html Msg
viewPreferredContactMethods isInherited preferredContactMethod =
    Html.ul [ Attrs.class "flex gap-2 p-2 rounded", Attrs.classList [ ( "bg-[#e8f3fc]", isInherited ) ] ]
        (allContactMethods
            |> List.map
                (\method ->
                    Html.button
                        [ Attrs.class "border rounded px-2 py-1"
                        , Attrs.classList
                            [ ( "bg-[#4ba0e8] border-transparent", method == preferredContactMethod )
                            , ( "hover:bg-[#d2e7f9]", method /= preferredContactMethod && not isInherited )
                            , ( "border-transparent", isInherited )
                            ]
                        , Attrs.disabled isInherited
                        , Events.onClick (PreferredContactMethodChanged method)
                        ]
                        [ Html.text (contactMethodToString method) ]
                )
        )


viewEmail : Bool -> String -> Html Msg
viewEmail isInherited email =
    viewInput
        { label = "e-mail"
        , disabled = isInherited
        , type_ = "email"
        , onChange = EmailChanged
        , value = email
        }


viewPhone : Bool -> String -> Html Msg
viewPhone isInherited phone =
    viewInput
        { label = "phone"
        , disabled = isInherited
        , type_ = "tel"
        , onChange = PhoneChanged
        , value = phone
        }


viewInput { label, disabled, type_, onChange, value } =
    Html.span [ Attrs.class "flex flex-col rounded px-2 py-1", Attrs.classList [ ( "bg-[#e8f3fc]", disabled ) ] ]
        [ Html.label [ Attrs.class "text-sm pl-1" ] [ Html.text label ]
        , Html.input
            [ Attrs.type_ type_
            , Attrs.class "border rounded px-2 py-1 focus:outline-none border-stone-400"
            , Attrs.classList [ ( "bg-[#e8f3fc]", disabled ) ]
            , Attrs.disabled disabled
            , Attrs.value value
            , Events.onInput onChange
            ]
            []
        ]
