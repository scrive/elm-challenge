module Main exposing (main)

import Browser
import Browser.Navigation as Navigation
import ContactDetails
import Data
import Html exposing (Html)
import Html.Attributes as Attributes
import Json.Decode as Decode
import Settings
import Tabs
import Tags
import Types.UserGroup as UserGroup
import Url



---- MODEL ----


type Model
    = Error ErrorType
    | Success SuccessModelData


type ErrorType
    = DecodeError Decode.Error


type alias SuccessModelData =
    { userGroup : UserGroup.Model
    , tabs : Tabs.Model
    }


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
                , tabs = Tabs.init
                }

        Err error ->
            error
                |> DecodeError
                |> Error
    , Cmd.none
    )



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
        Success ({ tabs, userGroup } as successData) ->
            (case msg of
                NoOp ->
                    ( successData
                    , Cmd.none
                    )

                TabsMsg tabsMsg ->
                    tabs
                        |> Tabs.update tabsMsg
                        |> Tuple.mapBoth
                            (\updatedTabs -> { successData | tabs = updatedTabs })
                            (Cmd.map TabsMsg)

                ContactDetailsMsg contactDetailsMsg ->
                    userGroup
                        |> UserGroup.getContactDetails
                        |> ContactDetails.update contactDetailsMsg
                        |> Tuple.mapBoth
                            (\contactDetails -> { successData | userGroup = UserGroup.updateContactDetails contactDetails userGroup })
                            (Cmd.map ContactDetailsMsg)

                SettingsMsg settingsMsg ->
                    userGroup
                        |> UserGroup.getSettings
                        |> Settings.update settingsMsg
                        |> Tuple.mapBoth
                            (\settings -> { successData | userGroup = UserGroup.updateSettings settings userGroup })
                            (Cmd.map SettingsMsg)

                TagsMsg tagsMsg ->
                    userGroup
                        |> UserGroup.getTags
                        |> Tags.update tagsMsg
                        |> Tuple.mapBoth
                            (\tags -> { successData | userGroup = UserGroup.updateTags tags userGroup })
                            (Cmd.map TagsMsg)
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
        Success ({ tabs } as successModelData) ->
            Html.div [ Attributes.class "flex flex-col w-[1024px] items-center mx-auto mt-16 mb-48" ]
                [ tabs
                    |> Tabs.view
                    |> Html.map TabsMsg
                , formView successModelData
                ]

        -- TODO error
        Error (DecodeError error) ->
            Html.div [ Attributes.class "flex flex-col w-[1024px] items-center mx-auto mt-16 mb-48" ]
                [ Html.text "Somethint went wrong ..."
                ]


formView : SuccessModelData -> Html Msg
formView { tabs, userGroup } =
    Html.div [ Attributes.class "p-4 rounded-lg bg-gray-50 dark:bg-gray-800" ]
        [ case tabs of
            Tabs.ContactDetails ->
                Html.text "Contact details"

            Tabs.Settings ->
                Html.text "Settings"

            Tabs.Tags ->
                userGroup
                    |> UserGroup.getTags
                    |> Tags.view
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
