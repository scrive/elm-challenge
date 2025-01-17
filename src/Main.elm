module Main exposing (main)

import Browser
import Data
import Dict exposing (Dict)
import Html exposing (Html)
import Html.Attributes as Attrs
import Html.Events as Events
import Json.Decode as Decode
import Json.Decode.Pipeline as Decode
import Modules.Contact as Contact
import Modules.Settings as Settings exposing (Settings)
import Modules.Tags as Tags



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
    , tagsForm : Tags.Model
    }


type Form
    = SettingsForm
    | ContactForm
    | TagsForm



-- User Group


type alias UserGroup =
    { id : String
    , parentId : String
    , name : String
    , children : List Children
    , settings : Settings
    , contactDetails : Contact.Details
    , tags : Dict String String
    }


emptyUserGroup : UserGroup
emptyUserGroup =
    { id = ""
    , parentId = ""
    , name = ""
    , children = []
    , settings = Settings.empty
    , contactDetails = Contact.empty
    , tags = Dict.empty
    }


userGroupDecoder : Decode.Decoder UserGroup
userGroupDecoder =
    Decode.succeed UserGroup
        |> Decode.required "id" Decode.string
        |> Decode.optional "parent_id" Decode.string ""
        |> Decode.required "name" Decode.string
        |> Decode.required "children" (Decode.list childrenDecoder)
        |> Decode.required "settings" Settings.decoder
        |> Decode.required "contact_details" Contact.decoder
        |> Decode.required "tags"
            (Decode.map
                (\tags ->
                    tags
                        |> List.map (\tag -> ( tag.name, tag.value ))
                        |> Dict.fromList
                )
                (Decode.list Tags.decoder)
            )


type alias Children =
    { id : String
    , name : String
    }


childrenDecoder : Decode.Decoder Children
childrenDecoder =
    Decode.succeed Children
        |> Decode.required "id" Decode.string
        |> Decode.required "name" Decode.string



-- Settings


init : ( Model, Cmd Msg )
init =
    let
        userGroup : UserGroup
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
                    not (String.isEmpty userGroup.settings.inheritedFrom)
                        && not (String.isEmpty userGroup.parentId)
                }
      , tagsForm =
            Tags.initialModel
                { tags = userGroup.tags
                , isInherited = not (String.isEmpty userGroup.parentId)
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
    | TagsFormMsg Tags.Msg
    | TagsSubmitted Tags.Model
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
                        { onSubmit = ContactSubmitted
                        , onClose = FormClosed
                        }
            in
            ( { model | contactForm = contactForm }
            , contactCmd
            )

        ContactSubmitted contact ->
            let
                newAddressDetails : Contact.Address
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

                toNewContactDetails : Contact.Details -> Contact.Details
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
                , contactForm =
                    Contact.initialModel
                        newAddressDetails
                        { isInherited = not (String.isEmpty userGroup.contactDetails.inheritedFrom) }
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
                        { isInherited = not (String.isEmpty userGroup.contactDetails.inheritedFrom) }
                , settingsForm =
                    Settings.initialModel
                        userGroup.settings.dataRetentionPolicy
                        { isInherited = not (String.isEmpty userGroup.settings.inheritedFrom) }
                , tagsForm =
                    Tags.initialModel
                        { tags = userGroup.tags
                        , isInherited = not (String.isEmpty userGroup.parentId)
                        }
              }
            , Cmd.none
            )

        SettingsFormMsg settingsMsg ->
            let
                ( settingsForm, settingsCmd ) =
                    Settings.update settingsMsg
                        model.settingsForm
                        { onSubmit = SettingsSubmitted
                        , onClose = FormClosed
                        , onFocus = NoOp
                        }
            in
            ( { model | settingsForm = settingsForm }
            , settingsCmd
            )

        SettingsSubmitted settings ->
            let
                newDataRetentionPolicy : Settings.DataRetentionPolicy
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
                , settingsForm =
                    Settings.initialModel
                        newDataRetentionPolicy
                        { isInherited = not (String.isEmpty userGroup.settings.inheritedFrom) }
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

        TagsFormMsg tagsMsg ->
            let
                ( tagsForm, tagsCmd ) =
                    Tags.update tagsMsg
                        model.tagsForm
                        { onSubmit = TagsSubmitted
                        , onClose = FormClosed
                        , onFocus = NoOp
                        }
            in
            ( { model | tagsForm = tagsForm }
            , tagsCmd
            )

        TagsSubmitted tags ->
            let
                newTags : Dict String String
                newTags =
                    tags.tags
                        |> Dict.values
                        |> List.map (\{ name, value } -> ( name, value ))
                        |> Dict.fromList

                toNewTags : UserGroup
                toNewTags =
                    { userGroup | tags = newTags }
            in
            ( { model
                | currentForm = Nothing
                , userGroup = toNewTags
                , tagsForm =
                    Tags.initialModel
                        { tags = newTags
                        , isInherited = not (String.isEmpty userGroup.parentId)
                        }
              }
            , Cmd.none
            )

        TagsEditClicked ->
            ( { model | currentForm = Just TagsForm }
            , Cmd.none
            )



---- VIEW ----


view : Model -> Html Msg
view { userGroup, currentForm, contactForm, settingsForm, tagsForm } =
    Html.div [ Attrs.class "flex flex-col items-center font-montserrat" ]
        (currentForm
            |> Maybe.map
                (\form ->
                    case form of
                        ContactForm ->
                            [ Contact.view contactForm |> Html.map ContactFormMsg ]

                        SettingsForm ->
                            [ Settings.view settingsForm |> Html.map SettingsFormMsg ]

                        TagsForm ->
                            [ Tags.view tagsForm |> Html.map TagsFormMsg ]
                )
            |> Maybe.withDefault
                [ Html.div [ Attrs.class "flex flex-col text-left my-2 w-full sm:w-3/6 border rounded" ]
                    [ Html.h1
                        [ Attrs.class "text-lg mb-2 bg-stone-100 border-b p-2.5" ]
                        [ Html.text "Group Details:" ]
                    , Html.h2
                        [ Attrs.class "text-md w-full p-2.5" ]
                        [ Html.text userGroup.name ]
                    ]
                , viewContact userGroup.contactDetails
                , viewSettings userGroup.settings
                , viewTags { tags = userGroup.tags }
                , Html.div [ Attrs.class "flex flex-col text-left my-2 w-full sm:w-3/6 border rounded" ]
                    [ Html.h1 [ Attrs.class "text-lg p-2.5 w-full" ] [ Html.text "Child groups:" ]
                    , Html.span []
                        (userGroup.children
                            |> List.map
                                (\childGroup ->
                                    Html.div [ Attrs.class "text-md p-2.5 w-full bg-stone-100 mb-1 " ]
                                        [ Html.text childGroup.name ]
                                )
                        )
                    ]
                ]
        )


viewTags : { tags : Dict String String } -> Html Msg
viewTags { tags } =
    Html.div [ Attrs.class "flex flex-col text-left my-2 w-full sm:w-3/6 border rounded p-2.5 gap-4" ]
        ([ Html.div [ Attrs.class "w-full flex flex-row justify-between gap-4 border-b pb-2" ]
            [ Html.h1 [ Attrs.class "text-lg font-semibold text-stone-800" ] [ Html.text "Tags" ]
            , Html.button
                [ Attrs.class "border border-transparent rounded px-2 py-1"
                , Attrs.class "bg-[#1e88e2] text-white outline-black hover:text-[#d2e7f9]"
                , Events.onClick TagsEditClicked
                ]
                [ Html.text "edit" ]
            ]
         ]
            ++ (if Dict.isEmpty tags then
                    [ Html.p [] [ Html.text "No tags found." ] ]

                else
                    [ Html.div [ Attrs.class "flex flex-wrap overflow-hidden gap-4" ]
                        (tags
                            |> Dict.toList
                            |> List.map
                                (\( name, value ) ->
                                    Html.p [ Attrs.class "bg-[#e8f3fc] w-fit p-1 rounded ellipsis" ]
                                        [ Html.text
                                            ([ name, value ]
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
    Html.div
        [ Attrs.class "flex flex-col text-left my-2 w-full"
        , Attrs.class "sm:w-3/6 border rounded p-2.5 gap-4"
        ]
        ([ Html.div [ Attrs.class "w-full flex flex-row justify-between gap-4 border-b pb-2" ]
            [ Html.h1 [ Attrs.class "text-lg font-semibold text-stone-800" ] [ Html.text "Settings" ]
            , Html.button
                [ Attrs.class "border border-transparent rounded px-2 py-1"
                , Attrs.class "bg-[#1e88e2] text-white outline-black hover:text-[#d2e7f9]"
                , Events.onClick SettingsEditClicked
                ]
                [ Html.text "edit" ]
            ]
         , Html.h1
            [ Attrs.class "text-md font-semibold text-stone-800" ]
            [ Html.text "Data retention policy:" ]
         ]
            ++ (Settings.activePolicies dataRetentionPolicy
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


viewContact : Contact.Details -> Html Msg
viewContact contactDetails =
    Html.div
        [ Attrs.class "flex flex-col text-left my-2 w-full"
        , Attrs.class "sm:w-3/6 border rounded p-2.5 gap-4"
        ]
        [ Html.div [ Attrs.class "w-full flex flex-row justify-between gap-4 border-b pb-2" ]
            [ Html.h1 [ Attrs.class "text-lg font-semibold text-stone-800" ] [ Html.text "Contact" ]
            , Html.button
                [ Attrs.class "border border-transparent rounded px-2 py-1"
                , Attrs.class "bg-[#1e88e2] text-white outline-black hover:text-[#d2e7f9]"
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
