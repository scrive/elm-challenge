module Main exposing (main)

import Browser
import Component.ContactDetails as ContactDetails
import Component.DataRetentionSettings as DataRetention
import Component.Tags as Tags
import Data.UserGroup
    exposing
        ( ContactDetails
        , UserGroup
        , fetchUserGroup
        , updateContactDetailsAddress
        , updateDataRetentionPolicy
        )
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Json.Decode as Decode
import Maybe.Extra as Maybe
import Views.Icons exposing (contactIcon, errorIcon, settingsIcon, tagIcon)



---- MODEL ----


type alias State =
    { userGroup : UserGroup
    , selectedTab : Tab
    , pageState : PageState
    }


type PageState
    = DataRetentionSettings DataRetention.Model
    | ContactDetails ContactDetails.Model
    | Tags Tags.Model


type Tab
    = SettingsTab
    | ContactTab
    | TagsTab


type Model
    = Valid State
    | Invalid String


initializeValidState : Tab -> UserGroup -> State
initializeValidState selectedTab userGroup =
    { userGroup = userGroup
    , selectedTab = selectedTab
    , pageState = initState selectedTab userGroup
    }


initState : Tab -> UserGroup -> PageState
initState tab userGroup =
    let
        dataRetentionPolicy =
            userGroup.settings.dataRetentionPolicy

        contactDetails =
            userGroup.contactDetails.address

        isSettingsInherited =
            userGroup.settings.inheritedFrom
                |> Maybe.isJust

        isContactDetailsInherited =
            userGroup.contactDetails.inheritedFrom
                |> Maybe.isJust
    in
    case tab of
        SettingsTab ->
            DataRetentionSettings <| DataRetention.init isSettingsInherited dataRetentionPolicy

        ContactTab ->
            ContactDetails <| ContactDetails.init isContactDetailsInherited contactDetails

        TagsTab ->
            Tags <| Tags.init userGroup.tags


init : ( Model, Cmd Msg )
init =
    let
        model =
            case fetchUserGroup of
                Ok userGroup ->
                    Valid <| initializeValidState SettingsTab userGroup

                Err e ->
                    Invalid <| Decode.errorToString e
    in
    ( model
    , Cmd.none
    )



---- UPDATE ----


type Msg
    = NoOp
    | SelectedTab Tab
    | SettingsMsg DataRetention.Msg
    | ContactDetailsMsg ContactDetails.Msg
    | TagsMsg Tags.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case model of
        Valid state ->
            let
                ( state_, cmd ) =
                    case msg of
                        SelectedTab tab ->
                            if tab == state.selectedTab then
                                ( state, Cmd.none )

                            else
                                ( { state | selectedTab = tab, pageState = initState tab state.userGroup }, Cmd.none )

                        SettingsMsg msg_ ->
                            case state.pageState of
                                DataRetentionSettings model_ ->
                                    case msg_ of
                                        DataRetention.ApplyButtonClicked ->
                                            let
                                                dataRetentionPolicy =
                                                    DataRetention.toDataRetentionPolicy model_

                                                updatedUserGroup =
                                                    updateDataRetentionPolicy dataRetentionPolicy state.userGroup

                                                pageState_ =
                                                    initState state.selectedTab updatedUserGroup
                                            in
                                            ( { state | pageState = pageState_, userGroup = updatedUserGroup }, Cmd.none )

                                        _ ->
                                            let
                                                ( pageModel, pageCmd ) =
                                                    DataRetention.update msg_ model_
                                            in
                                            ( { state | pageState = DataRetentionSettings pageModel }, pageCmd |> Cmd.map SettingsMsg )

                                _ ->
                                    ( state, Cmd.none )

                        ContactDetailsMsg msg_ ->
                            case state.pageState of
                                ContactDetails model_ ->
                                    case msg_ of
                                        ContactDetails.ApplyButtonClicked ->
                                            let
                                                contactDetailsAddress =
                                                    ContactDetails.toContactDetailsAddress model_

                                                updatedUserGroup =
                                                    updateContactDetailsAddress contactDetailsAddress state.userGroup

                                                pageState_ =
                                                    initState state.selectedTab updatedUserGroup
                                            in
                                            ( { state | pageState = pageState_, userGroup = updatedUserGroup }, Cmd.none )

                                        _ ->
                                            let
                                                ( pageModel, pageCmd ) =
                                                    ContactDetails.update msg_ model_
                                            in
                                            ( { state | pageState = ContactDetails pageModel }, pageCmd |> Cmd.map ContactDetailsMsg )

                                _ ->
                                    ( state, Cmd.none )

                        TagsMsg msg_ ->
                            case state.pageState of
                                Tags model_ ->
                                    case msg_ of
                                        Tags.ApplyButtonClicked ->
                                            let
                                                tags =
                                                    List.map Tags.fromTagField model_.newTags

                                                userGroup =
                                                    state.userGroup

                                                updatedUserGroup =
                                                    { userGroup | tags = tags }

                                                pageState_ =
                                                    initState state.selectedTab updatedUserGroup
                                            in
                                            ( { state | pageState = pageState_, userGroup = updatedUserGroup }, Cmd.none )

                                        _ ->
                                            let
                                                ( pageModel, pageCmd ) =
                                                    Tags.update msg_ model_
                                            in
                                            ( { state | pageState = Tags pageModel }, pageCmd |> Cmd.map TagsMsg )

                                _ ->
                                    ( state, Cmd.none )

                        _ ->
                            ( state, Cmd.none )
            in
            ( Valid state_, cmd )

        _ ->
            ( model, Cmd.none )



---- VIEW ----


header : String -> Html msg
header text_ =
    span [ class "p-2 text-5xl font-extrabold text-transparent bg-clip-text bg-gradient-to-br from-slate-400 to-slate-800" ]
        [ text text_ ]


subheader : String -> Html msg
subheader text_ =
    span [ class "p-2 text-2xl font-extrabold text-slate-800" ]
        [ text text_ ]



--- Main view functions ---


error : String -> Html Msg
error e =
    div [ class "flex items-center w-6/12 p-4 mb-2 text-sm text-red-800 rounded-lg bg-red-50", attribute "role" "alert" ]
        [ errorIcon
        , span [ class "sr-only" ] [ text "Info" ]
        , div []
            [ text <| "Error decoding data: " ++ e ]
        ]


tabItem : Tab -> Tab -> (Bool -> Html Msg) -> String -> Html Msg
tabItem selectedTab tabType icon name =
    let
        addClass =
            if isSelected then
                "text-blue-600 border-blue-600 active"

            else
                "border-transparent hover:text-gray-600 hover:border-gray-300"

        isSelected =
            selectedTab == tabType
    in
    li [ class "me-2" ]
        [ a
            [ href "#"
            , class <| "inline-flex items-center justify-center p-4 border-b-2 rounded-t-lg group " ++ addClass
            , onClick <| SelectedTab tabType
            ]
            [ icon isSelected
            , text name
            ]
        ]


viewTabs : Tab -> Html Msg
viewTabs selectedTab =
    div [ class "text-sm font-medium text-center w-full text-gray-500 border-b border-gray-200 mb-6" ]
        [ ul [ class "flex flex-wrap -mb-px" ]
            [ tabItem selectedTab SettingsTab settingsIcon "Settings"
            , tabItem selectedTab ContactTab contactIcon "Contact details"
            , tabItem selectedTab TagsTab tagIcon "Tags"
            ]
        ]


viewTabContent : State -> Html Msg
viewTabContent state =
    case state.pageState of
        DataRetentionSettings m ->
            DataRetention.view m
                |> Html.map SettingsMsg

        ContactDetails m ->
            ContactDetails.view m
                |> Html.map ContactDetailsMsg

        Tags m ->
            Tags.view m
                |> Html.map TagsMsg


view : Model -> Html Msg
view model =
    case model of
        Valid state ->
            div [ class "flex flex-col w-[1024px] items-start mx-auto mt-16 mb-48 px-12" ]
                [ header "User group"
                , subheader state.userGroup.name
                , viewTabs state.selectedTab
                , viewTabContent state
                ]

        Invalid e ->
            div [ class "flex flex-col w-[1024px] items-center mx-auto mt-16 mb-48" ]
                [ header "User group"
                , error e
                ]



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
