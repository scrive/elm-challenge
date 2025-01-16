module Main exposing (..)

import Browser
import Data
import Html exposing (Html)
import Html.Attributes as Attrs
import Html.Events as Events
import Json.Decode as Decode
import Json.Decode.Pipeline as Decode
import Modules.Contact as Contact
import Modules.Settings as Settings



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
    , settingsForm : Settings.Model
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
            Contact.initialModel
                userGroup.contactDetails.address
                { isInherited =
                    not (String.isEmpty userGroup.contactDetails.inheritedFrom)
                        && not (String.isEmpty userGroup.parentId)
                }
      , settingsForm =
            Settings.initialModel
                userGroup.settings.dataRetentionPolicy
                { isInherited =
                    not (String.isEmpty userGroup.contactDetails.inheritedFrom)
                        && not (String.isEmpty userGroup.parentId)
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
    | SettingsFormMsg Settings.Msg
    | SettingsSubmitted Settings.Model
    | SettingsEditClicked
    | TagsEditClicked


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
            ( { model
                | currentForm = Nothing
                , contactForm =
                    Contact.initialModel
                        userGroup.contactDetails.address
                        { isInherited =
                            not (String.isEmpty userGroup.contactDetails.inheritedFrom)
                                && not (String.isEmpty userGroup.parentId)
                        }
                , settingsForm =
                    Settings.initialModel
                        userGroup.settings.dataRetentionPolicy
                        { isInherited =
                            not (String.isEmpty userGroup.contactDetails.inheritedFrom)
                                && not (String.isEmpty userGroup.parentId)
                        }
              }
            , Cmd.none
            )

        SettingsFormMsg settingsMsg ->
            let
                ( settingsForm, settingsCmd ) =
                    Settings.update settingsMsg
                        model.settingsForm
                        { onSubmit = SettingsSubmitted, onClose = FormClosed, onFocus = NoOp }
            in
            ( { model | settingsForm = settingsForm }
            , settingsCmd
            )

        SettingsSubmitted settings ->
            let
                newDataRetentionPolicy : DataRetentionPolicy
                newDataRetentionPolicy =
                    { idleDocTimeOutPreparation = settings.idleDocTimeOutPreparation
                    , idleDocTimeOutClosed = settings.idleDocTimeOutClosed
                    , idleDocTimeOutCancelled = settings.idleDocTimeOutCancelled
                    , idleDocTimeOutTimedOut = settings.idleDocTimeOutTimedOut
                    , idleDocTimeOutRejected = settings.idleDocTimeOutRejected
                    , idleDocTimeOutError = settings.idleDocTimeOutError
                    , immediateTrash = settings.immediateTrash
                    }

                toNewSettings : Settings -> Settings
                toNewSettings settings_ =
                    { settings_ | dataRetentionPolicy = newDataRetentionPolicy }
            in
            ( { model
                | currentForm = Nothing
                , userGroup =
                    { userGroup
                        | settings =
                            toNewSettings userGroup.settings
                    }
              }
            , Cmd.none
            )

        SettingsEditClicked ->
            ( { model | currentForm = Just SettingsForm }, Cmd.none )

        TagsEditClicked ->
            ( model, Cmd.none )



---- VIEW ----


view : Model -> Html Msg
view { userGroup, currentForm, contactForm, settingsForm } =
    Html.div [ Attrs.class "flex flex-col items-center font-montserrat" ]
        (currentForm
            |> Maybe.map
                (\form ->
                    case form of
                        ContactForm ->
                            [ Contact.view contactForm |> Html.map ContactFormMsg ]

                        SettingsForm ->
                            [ Settings.view settingsForm |> Html.map SettingsFormMsg ]
                )
            |> Maybe.withDefault
                [ Html.div [ Attrs.class "flex flex-col text-left my-2 w-full sm:w-3/6 border rounded" ]
                    [ Html.h1 [ Attrs.class "text-lg mb-2 bg-stone-100 border-b p-2.5" ] [ Html.text "Group Details:" ]
                    , Html.h2 [ Attrs.class "text-md w-full p-2.5" ] [ Html.text userGroup.name ]
                    ]
                , viewContact userGroup.contactDetails
                , viewSettings userGroup.settings
                , viewTags userGroup.tags
                , Html.div [ Attrs.class "flex flex-col text-left my-2 w-full sm:w-3/6 border rounded" ]
                    [ Html.h1 [ Attrs.class "text-lg p-2.5 w-full" ] [ Html.text "Child groups:" ]
                    , Html.span []
                        (userGroup.children
                            |> List.map (\childGroup -> Html.div [ Attrs.class "text-md p-2.5 w-full bg-stone-100 mb-1 " ] [ Html.text childGroup.name ])
                        )
                    ]
                ]
        )


viewTags : List Tag -> Html Msg
viewTags tags =
    Html.div [ Attrs.class "flex flex-col text-left my-2 w-full sm:w-3/6 border rounded p-2.5 gap-4" ]
        ([ Html.div [ Attrs.class "w-full flex flex-row justify-between gap-4 border-b pb-2" ]
            [ Html.h1 [ Attrs.class "text-lg font-semibold text-stone-800" ] [ Html.text "Tags" ]
            , Html.button
                [ Attrs.class "border border-transparent rounded px-2 py-1 bg-[#1e88e2] text-white outline-black hover:text-[#d2e7f9]"
                , Events.onClick TagsEditClicked
                ]
                [ Html.text "edit" ]
            ]
         ]
            ++ (if List.isEmpty tags then
                    [ Html.p [] [ Html.text "No tags found." ] ]

                else
                    [ Html.div [ Attrs.class "flex gap-4" ]
                        (tags
                            |> List.map
                                (\tag ->
                                    Html.p [ Attrs.class "bg-[#e8f3fc] w-fit p-1 rounded" ]
                                        [ Html.text
                                            ([ tag.name, tag.value ]
                                                |> List.filter (not << String.isEmpty)
                                                |> String.join " : "
                                            )
                                        ]
                                )
                        )
                    ]
               )
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
            ++ (Settings.existingPolicies dataRetentionPolicy
                    |> List.map
                        (\( policy, value ) ->
                            Html.div [ Attrs.class "w-full flex flex-row gap-4" ]
                                [ Html.p [] [ Html.text (Settings.policyToString policy ++ ":") ]
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
