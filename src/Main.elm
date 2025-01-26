module Main exposing (main)

import Browser
import Browser.Navigation as Navigation
import Data
import Data.UserGroup as UserGroup
import Html exposing (Html)
import Html.Attributes as Attributes
import Json.Decode as Decode
import Section.ContactDetails as ContactDetails
import Section.Settings as Settings
import Section.Tags as Tags
import Tabs
import Url



---- MODEL ----


type Model
    = Error ErrorType
    | Success SuccessModelData


type ErrorType
    = DecoderError Decode.Error


type alias SuccessModelData =
    { userGroup : UserGroup.Model
    , section : Section
    }


type Section
    = ContactDetailsSection ContactDetails.Model
    | SettingsSection Settings.Model
    | TagsSection Tags.Model


type alias Flags =
    ()


init : Flags -> Url.Url -> Navigation.Key -> ( Model, Cmd Msg )
init _ _ _ =
    let
        userGroupDecoderResult =
            Decode.decodeString UserGroup.decode Data.userGroup
    in
    ( case userGroupDecoderResult of
        Ok userGroup ->
            Success
                { userGroup = userGroup
                , section = initSection Tabs.init
                }

        Err error ->
            error
                |> DecoderError
                |> Error
    , Cmd.none
    )


initSection : Tabs.Model -> Section
initSection tabsModel =
    case tabsModel of
        Tabs.ContactDetails ->
            ContactDetailsSection ContactDetails.init

        Tabs.Settings ->
            SettingsSection Settings.init

        Tabs.Tags ->
            TagsSection Tags.init


sectionToTab : Section -> Tabs.Model
sectionToTab section =
    case section of
        ContactDetailsSection _ ->
            Tabs.ContactDetails

        SettingsSection _ ->
            Tabs.Settings

        TagsSection _ ->
            Tabs.Tags



---- UPDATE ----


type Msg
    = NoOp
    | TabsMsg Tabs.Msg
    | ContactDetailsMsg ContactDetails.Msg
    | SettingsMsg Settings.Msg
    | TagsMsg Tags.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case model of
        Success ({ section, userGroup } as successData) ->
            (case ( msg, section ) of
                ( TabsMsg tabsMsg, _ ) ->
                    let
                        ( updatedTabsModel, cmd ) =
                            Tabs.update tabsMsg
                    in
                    ( { successData | section = initSection updatedTabsModel }
                    , Cmd.map TabsMsg cmd
                    )

                ( ContactDetailsMsg sectionMsg, ContactDetailsSection sectionModel ) ->
                    let
                        ( updatedModel, cmd, updatedContactDetails ) =
                            ContactDetails.update (UserGroup.getContactDetails userGroup) sectionMsg sectionModel
                    in
                    ( { successData
                        | section = ContactDetailsSection updatedModel
                        , userGroup = UserGroup.updateContactDetails updatedContactDetails userGroup
                      }
                    , Cmd.map ContactDetailsMsg cmd
                    )

                ( SettingsMsg sectionMsg, SettingsSection sectionModel ) ->
                    let
                        ( updatedModel, cmd, updatedSettings ) =
                            Settings.update (UserGroup.getSettings userGroup) sectionMsg sectionModel
                    in
                    ( { successData
                        | section = SettingsSection updatedModel
                        , userGroup = UserGroup.updateSettings updatedSettings userGroup
                      }
                    , Cmd.map SettingsMsg cmd
                    )

                ( TagsMsg sectionMsg, TagsSection sectionModel ) ->
                    let
                        ( updatedModel, cmd, updatedTags ) =
                            Tags.update (UserGroup.getTags userGroup) sectionMsg sectionModel
                    in
                    ( { successData
                        | section = TagsSection updatedModel
                        , userGroup = UserGroup.updateTags updatedTags userGroup
                      }
                    , Cmd.map TagsMsg cmd
                    )

                _ ->
                    ( successData, Cmd.none )
            )
                |> Tuple.mapFirst Success

        Error _ ->
            ( model
            , Cmd.none
            )



---- VIEW ----


view : Model -> Html Msg
view model =
    case model of
        Success ({ section } as successModelData) ->
            Html.div [ Attributes.class "flex flex-col w-[1024px] items-center mx-auto mt-16 mb-48" ]
                [ section
                    |> sectionToTab
                    |> Tabs.view
                    |> Html.map TabsMsg
                , formView successModelData
                ]

        -- TODO error
        Error (DecoderError error) ->
            Html.div [ Attributes.class "flex flex-col w-[1024px] items-center mx-auto mt-16 mb-48" ]
                [ Html.text "Something went wrong ..."
                ]


formView : SuccessModelData -> Html Msg
formView { section, userGroup } =
    Html.div [ Attributes.class "p-4 rounded-lg bg-gray-50 dark:bg-gray-800" ]
        [ case section of
            ContactDetailsSection contactDetailsModel ->
                ContactDetails.view (UserGroup.getContactDetails userGroup) contactDetailsModel
                    |> Html.map ContactDetailsMsg

            SettingsSection settingsModel ->
                Settings.view (UserGroup.getSettings userGroup) settingsModel
                    |> Html.map SettingsMsg

            TagsSection tagsModel ->
                Tags.view (UserGroup.getTags userGroup) tagsModel
                    |> Html.map TagsMsg
        ]



---- ROGRAM ----


main : Program () Model Msg
main =
    Browser.application
        { view =
            \model ->
                { title = "Scrive elm challenge task"
                , body = [ view model ]
                }
        , init = init
        , update = update
        , subscriptions = always Sub.none
        , onUrlRequest = always NoOp
        , onUrlChange = always NoOp
        }
