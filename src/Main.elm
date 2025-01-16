module Main exposing (..)

import Browser
import Data
import Html exposing (Html)
import Html.Attributes as Attrs
import Html.Events as Events
import Json.Decode as Decode
import Json.Decode.Pipeline as Decode
import Modules.Contact as Contact



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
    , currentForm : Maybe Form
    , contactForm : Contact.Model
    }


type Form
    = SettingsForm
    | ContactForm



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
        |> Decode.optional "parent_id" Decode.string ""
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


existingPolicies : DataRetentionPolicy -> List ( String, Int )
existingPolicies dataRetentionPolicies =
    [ dataRetentionPolicies.idleDocTimeOutPreparation |> Maybe.map (\value -> ( "preparation", value ))
    , dataRetentionPolicies.idleDocTimeOutClosed |> Maybe.map (\value -> ( "closed", value ))
    , dataRetentionPolicies.idleDocTimeOutCancelled |> Maybe.map (\value -> ( "cancelled", value ))
    , dataRetentionPolicies.idleDocTimeOutTimedOut |> Maybe.map (\value -> ( "timed out", value ))
    , dataRetentionPolicies.idleDocTimeOutRejected |> Maybe.map (\value -> ( "rejected", value ))
    , dataRetentionPolicies.idleDocTimeOutError |> Maybe.map (\value -> ( "error", value ))
    ]
        |> List.filterMap identity


type Policy
    = Preparation
    | Closed
    | Cancelled
    | TimedOut
    | Rejected
    | Error


policyToString : Policy -> String
policyToString policy =
    case policy of
        Preparation ->
            "preparation"

        Closed ->
            "closed"

        Cancelled ->
            "cancelled"

        TimedOut ->
            "timedOut"

        Rejected ->
            "rejected"

        Error ->
            "error"


missingPolicies : DataRetentionPolicy -> List Policy
missingPolicies dataRetentionPolicies =
    [ ( dataRetentionPolicies.idleDocTimeOutPreparation, Preparation )
    , ( dataRetentionPolicies.idleDocTimeOutClosed, Closed )
    , ( dataRetentionPolicies.idleDocTimeOutCancelled, Cancelled )
    , ( dataRetentionPolicies.idleDocTimeOutTimedOut, TimedOut )
    , ( dataRetentionPolicies.idleDocTimeOutRejected, Rejected )
    , ( dataRetentionPolicies.idleDocTimeOutError, Error )
    ]
        |> List.filter (Tuple.first >> (==) Nothing)
        |> List.map Tuple.second


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
    { preferredContactMethod : Contact.PreferredContactMethod
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
    { preferredContactMethod = Contact.Email
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
        |> Decode.required "preferred_contact_method" Contact.preferredContactMethodDecoder
        |> Decode.optional "email" Decode.string ""
        |> Decode.optional "phone" Decode.string ""
        |> Decode.optional "company_name" Decode.string ""
        |> Decode.optional "address" Decode.string ""
        |> Decode.optional "zip" Decode.string ""
        |> Decode.optional "city" Decode.string ""
        |> Decode.optional "country" Decode.string ""


init : ( Model, Cmd Msg )
init =
    let
        userGroup =
            Decode.decodeString userGroupDecoder Data.userGroup
                |> Result.toMaybe
                |> Maybe.withDefault emptyUserGroup
    in
    ( { userGroup = userGroup
      , currentForm = Nothing
      , contactForm =
            { preferredContactMethod = userGroup.contactDetails.address.preferredContactMethod
            , email = userGroup.contactDetails.address.email
            , phone = userGroup.contactDetails.address.phone
            , companyName = userGroup.contactDetails.address.companyName
            , address = userGroup.contactDetails.address.address
            , zip = userGroup.contactDetails.address.zip
            , city = userGroup.contactDetails.address.city
            , country = userGroup.contactDetails.address.country
            , isInherited = not (String.isEmpty userGroup.contactDetails.inheritedFrom) && not (String.isEmpty userGroup.parentId)
            , error = Nothing
            }
      }
    , Cmd.none
    )



---- UPDATE ----


type Msg
    = NoOp
    | ContactFormMsg Contact.Msg
    | ContactSubmitted Contact.Model
    | ContactEditClicked
    | FormClosed
    | SettingsEditClicked
    | AddPolicy Policy
    | ImmediateTrashChecked Bool


update : Msg -> Model -> ( Model, Cmd Msg )
update msg ({ userGroup } as model) =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        ContactFormMsg contactMsg ->
            let
                ( contactForm, contactCmd ) =
                    Contact.update contactMsg
                        model.contactForm
                        { onSubmit = ContactSubmitted, onClose = FormClosed }
            in
            ( { model | contactForm = contactForm }
            , contactCmd
            )

        ContactSubmitted contact ->
            let
                newAddressDetails : Address
                newAddressDetails =
                    { preferredContactMethod = contact.preferredContactMethod
                    , email = contact.email
                    , phone = contact.phone
                    , companyName = contact.companyName
                    , address = contact.address
                    , zip = contact.zip
                    , city = contact.city
                    , country = contact.country
                    }

                toNewContactDetails : ContactDetails -> ContactDetails
                toNewContactDetails contactDetails =
                    { contactDetails | address = newAddressDetails }
            in
            ( { model
                | currentForm = Nothing
                , userGroup =
                    { userGroup
                        | contactDetails =
                            toNewContactDetails userGroup.contactDetails
                    }
              }
            , Cmd.none
            )

        ContactEditClicked ->
            ( { model | currentForm = Just ContactForm }
            , Cmd.none
            )

        FormClosed ->
            ( { model | currentForm = Nothing }, Cmd.none )

        SettingsEditClicked ->
            ( { model | currentForm = Just SettingsForm }, Cmd.none )

        AddPolicy policy ->
            let
                toNewPolicySettings settings =
                    { settings | dataRetentionPolicy = addPolicy policy settings.dataRetentionPolicy }
            in
            ( { model
                | userGroup =
                    { userGroup
                        | settings =
                            toNewPolicySettings userGroup.settings
                    }
              }
            , Cmd.none
            )

        ImmediateTrashChecked bool ->
            let
                toNewPolicySettings settings =
                    { settings | dataRetentionPolicy = toggleImmediateTresh bool settings.dataRetentionPolicy }
            in
            ( { model
                | userGroup =
                    { userGroup
                        | settings =
                            toNewPolicySettings userGroup.settings
                    }
              }
            , Cmd.none
            )


addPolicy : Policy -> DataRetentionPolicy -> DataRetentionPolicy
addPolicy policy dataRetentionPolicy =
    case policy of
        Preparation ->
            { dataRetentionPolicy | idleDocTimeOutPreparation = Just 1 }

        Closed ->
            { dataRetentionPolicy | idleDocTimeOutClosed = Just 1 }

        Cancelled ->
            { dataRetentionPolicy | idleDocTimeOutCancelled = Just 1 }

        TimedOut ->
            { dataRetentionPolicy | idleDocTimeOutTimedOut = Just 1 }

        Rejected ->
            { dataRetentionPolicy | idleDocTimeOutRejected = Just 1 }

        Error ->
            { dataRetentionPolicy | idleDocTimeOutError = Just 1 }


toggleImmediateTresh : Bool -> DataRetentionPolicy -> DataRetentionPolicy
toggleImmediateTresh bool dataRetentionPolicy =
    { dataRetentionPolicy | immediateTrash = bool }



---- VIEW ----


view : Model -> Html Msg
view { userGroup, currentForm, contactForm } =
    Html.div [ Attrs.class "flex flex-col items-center font-montserrat" ]
        (currentForm
            |> Maybe.map
                (\form ->
                    case form of
                        ContactForm ->
                            [ Contact.view contactForm |> Html.map ContactFormMsg ]

                        SettingsForm ->
                            [ viewSettingsForm userGroup.parentId userGroup.settings ]
                )
            |> Maybe.withDefault
                [ Html.div [ Attrs.class "flex flex-col text-left my-2 w-full sm:w-3/6 border rounded" ]
                    [ Html.h1 [ Attrs.class "text-lg mb-2 bg-stone-100 border-b p-2.5" ] [ Html.text "Group Details:" ]
                    , Html.h2 [ Attrs.class "text-md w-full p-2.5" ] [ Html.text userGroup.name ]
                    ]
                , viewContact userGroup.contactDetails
                , viewSettings userGroup.settings
                , Html.div [ Attrs.class "flex flex-col text-left my-2 w-full sm:w-3/6 border rounded" ]
                    [ Html.h1 [ Attrs.class "text-lg p-2.5 w-full" ] [ Html.text "Child groups:" ]
                    , Html.span []
                        (userGroup.children
                            |> List.map (\childGroup -> Html.div [ Attrs.class "text-md p-2.5 w-full bg-stone-100 mb-1 " ] [ Html.text childGroup.name ])
                        )
                    ]
                ]
        )


viewSettingsForm : String -> Settings -> Html Msg
viewSettingsForm parentId { inheritedFrom, dataRetentionPolicy } =
    let
        isInherited : Bool
        isInherited =
            not (String.isEmpty inheritedFrom) || not (String.isEmpty parentId)
    in
    Html.form
        [ Attrs.class "flex flex-col gap-4 my-2 p-2.5 w-full sm:w-6/12 md:w-3/6 lg:w-2/6 border rounded whitespace-nowrap text-ellipsis overflow-hidden"
        , Events.onSubmit FormClosed
        ]
        ([ [ Html.h1 [ Attrs.class "text-md font-semibold text-stone-700 pl-1" ]
                [ Html.text "Data retention policy" ]
           ]
         , existingPolicies dataRetentionPolicy
            |> List.map
                (\( policy, value ) ->
                    Html.span
                        [ Attrs.class "flex flex-row rounded p-2.5 justify-between items-center"
                        , Attrs.classList [ ( "bg-[#e8f3fc]", isInherited ) ]
                        ]
                        [ Html.label [ Attrs.class "text-md pl-1 font-semibold text-stone-700" ]
                            [ Html.text (policy ++ ":") ]
                        , Html.input
                            [ Attrs.type_ "number"
                            , Attrs.class "border rounded px-2 py-1 focus:outline-none border-stone-400 w-24 text-md text-right text-normal text-stone-700 appearance-none"
                            , Attrs.class "[appearance:textfield] [&::-webkit-outer-spin-button]:appearance-none [&::-webkit-inner-spin-button]:appearance-none"
                            , Attrs.classList [ ( "bg-[#e8f3fc]", isInherited ) ]
                            , Attrs.disabled isInherited
                            , Attrs.value (String.fromInt value)
                            ]
                            []
                        ]
                )
         , [ Html.span
                [ Attrs.class "flex flex-row rounded p-2.5 justify-between"
                , Attrs.classList [ ( "bg-[#e8f3fc]", isInherited ) ]
                ]
                [ Html.label [ Attrs.class "text-md font-semibold text-stone-700 pl-1" ]
                    [ Html.text "immediate trash" ]
                , Html.input
                    [ Attrs.type_ "checkbox"
                    , Attrs.class "border rounded px-2 py-1 border-stone-400 w-4"
                    , Attrs.classList [ ( "bg-[#e8f3fc]", isInherited ) ]
                    , Attrs.disabled isInherited
                    , Events.onCheck ImmediateTrashChecked
                    , Attrs.checked dataRetentionPolicy.immediateTrash
                    ]
                    []
                ]
           ]
         , if not isInherited then
            missingPolicies
                dataRetentionPolicy
                |> List.map
                    (\policy ->
                        Html.span
                            [ Attrs.class "flex flex-row rounded p-2.5 justify-between items-center bg-stone-100"
                            , Attrs.classList [ ( "bg-[#e8f3fc]", isInherited ) ]
                            ]
                            [ Html.label [ Attrs.class "text-md pl-1 font-semibold text-stone-700" ]
                                [ Html.text (policyToString policy ++ ":") ]
                            , Html.button
                                [ Attrs.type_ "button"
                                , Attrs.class "border rounded px-2 py-1 hover:bg-[#d2e7f9] border-stone-400 text-md text-right text-normal text-stone-700"
                                , Attrs.classList [ ( "bg-[#e8f3fc]", isInherited ) ]
                                , Attrs.disabled isInherited
                                , Events.onClick (AddPolicy policy)
                                ]
                                [ Html.text "add" ]
                            ]
                    )

           else
            []
         , [ Html.span
                [ Attrs.class "flex flex-row gap-4"
                , Attrs.classList
                    [ ( "justify-end", not isInherited )
                    , ( "justify-center", isInherited )
                    ]
                ]
                (if isInherited then
                    [ Html.button
                        [ Attrs.class "border border-black black rounded px-2 py-1 text-black w-2/6 hover:bg-[#d2e7f9]"
                        , Attrs.type_ "button"
                        , Events.onClick FormClosed
                        ]
                        [ Html.text "close" ]
                    ]

                 else
                    [ Html.button
                        [ Attrs.class "border border-black black rounded px-2 py-1 text-black hover:bg-[#d2e7f9]"
                        , Attrs.type_ "button"
                        , Events.onClick FormClosed
                        ]
                        [ Html.text "cancel" ]
                    , Html.button
                        [ Attrs.class "border border-transparent rounded px-2 py-1 bg-[#1e88e2] text-white outline-black hover:text-[#d2e7f9]"
                        , Attrs.type_ "submit"
                        , Events.onClick FormClosed
                        ]
                        [ Html.text "apply" ]
                    ]
                )
           ]
         ]
            |> List.concat
        )


viewSettings : Settings -> Html Msg
viewSettings { dataRetentionPolicy } =
    Html.div [ Attrs.class "flex flex-col text-left my-2 w-full sm:w-3/6 border rounded p-2.5 gap-4" ]
        ([ Html.div [ Attrs.class "w-full flex flex-row justify-between gap-4 border-b pb-2" ]
            [ Html.h1 [ Attrs.class "text-lg font-semibold text-stone-800" ] [ Html.text "Settings" ]
            , Html.button
                [ Attrs.class "border border-transparent rounded px-2 py-1 bg-[#1e88e2] text-white outline-black hover:text-[#d2e7f9]"
                , Events.onClick SettingsEditClicked
                ]
                [ Html.text "edit" ]
            ]
         , Html.h1 [ Attrs.class "text-md font-semibold text-stone-800" ] [ Html.text "Data retention policy:" ]
         ]
            ++ (existingPolicies dataRetentionPolicy
                    |> List.map
                        (\( policy, value ) ->
                            Html.div [ Attrs.class "w-full flex flex-row gap-4" ]
                                [ Html.p [] [ Html.text (policy ++ ":") ]
                                , Html.p [] [ Html.text (String.fromInt value) ]
                                ]
                        )
               )
            ++ [ Html.div [ Attrs.class "w-full flex flex-row gap-4" ]
                    [ Html.p [] [ Html.text "immediate trash:" ]
                    , Html.p []
                        [ Html.text
                            (if dataRetentionPolicy.immediateTrash then
                                "yes"

                             else
                                "no"
                            )
                        ]
                    ]
               ]
        )


viewContact : ContactDetails -> Html Msg
viewContact contactDetails =
    Html.div [ Attrs.class "flex flex-col text-left my-2 w-full sm:w-3/6 border rounded p-2.5 gap-4" ]
        [ Html.div [ Attrs.class "w-full flex flex-row justify-between gap-4 border-b pb-2" ]
            [ Html.h1 [ Attrs.class "text-lg font-semibold text-stone-800" ] [ Html.text "Contact" ]
            , Html.button
                [ Attrs.class "border border-transparent rounded px-2 py-1 bg-[#1e88e2] text-white outline-black hover:text-[#d2e7f9]"
                , Events.onClick ContactEditClicked
                ]
                [ Html.text "edit" ]
            ]
        , Html.div [ Attrs.class "w-full flex flex-col" ]
            ([ Html.p [ Attrs.class "text-md font-semibold text-stone-800" ]
                [ Html.text contactDetails.address.companyName ]
             ]
                ++ (case contactDetails.address.preferredContactMethod of
                        Contact.Email ->
                            [ Html.p [ Attrs.class "text-md font-semibold text-stone-800" ]
                                [ Html.text contactDetails.address.email ]
                            ]

                        Contact.Phone ->
                            [ Html.p [ Attrs.class "text-md font-semibold text-stone-800" ]
                                [ Html.text contactDetails.address.phone ]
                            ]

                        Contact.Post ->
                            [ Html.p [ Attrs.class "text-md font-semibold text-stone-800" ]
                                [ Html.text contactDetails.address.address ]
                            , Html.p [ Attrs.class "text-md font-semibold text-stone-800" ]
                                [ Html.text contactDetails.address.zip ]
                            , Html.p [ Attrs.class "text-md font-semibold text-stone-800" ]
                                [ Html.text contactDetails.address.city ]
                            , Html.p [ Attrs.class "text-md font-semibold text-stone-800" ]
                                [ Html.text contactDetails.address.country ]
                            ]
                   )
            )
        ]
