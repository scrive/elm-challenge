module Main exposing (main)

import Browser
import Browser.Navigation as Navigation
import Data
import Form.ContactDetails as ContactDetails
import Form.Settings as Settings
import Form.Tags as Tags
import Html exposing (Html)
import Html.Attributes as Attributes
import Json.Decode as Decode
import Types.UserGroup as UserGroup
import Url



---- MODEL ----


type Model
    = Error ErrorType
    | Success SuccessData


type ErrorType
    = DecodeError Decode.Error


type alias SuccessData =
    { userGroup : UserGroup.Model
    , activeTab : Tab
    }


type Tab
    = ContactDetails
    | Settings
    | Tags


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
                , activeTab = ContactDetails
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
    | ChangedTab Tab
    | ContactDetailsMsg ContactDetails.Msg
    | SettingsMsg Settings.Msg
    | TagsMsg Tags.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case model of
        Success ({ userGroup } as successData) ->
            (case msg of
                NoOp ->
                    ( successData
                    , Cmd.none
                    )

                ChangedTab tab ->
                    ( { successData | activeTab = tab }
                    , Cmd.none
                    )

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


header : String -> Html msg
header text =
    Html.span [ Attributes.class "p-2 text-5xl font-extrabold text-transparent bg-clip-text bg-gradient-to-br from-slate-400 to-slate-800" ]
        [ Html.text text ]


subheader : String -> Html msg
subheader text =
    Html.span [ Attributes.class "p-2 text-2xl font-extrabold text-slate-800" ]
        [ Html.text text ]


view : Model -> Html Msg
view model =
    case model of
        Success _ ->
            Html.div [ Attributes.class "flex flex-col w-[1024px] items-center mx-auto mt-16 mb-48" ]
                [ header "Let's start your task"
                , subheader "Here are your data:"
                , Html.pre [ Attributes.class "my-8 py-4 px-12 text-sm bg-slate-100 font-mono shadow rounded" ] [ Html.text Data.userGroup ]
                , header "Now turn them into form."
                , subheader "See README for details of the task. Good luck 🍀 "
                ]

        Error (DecodeError error) ->
            let
                _ =
                    Debug.log "log" error
            in
            Html.div [ Attributes.class "flex flex-col w-[1024px] items-center mx-auto mt-16 mb-48" ]
                [ Html.text "Somethint went wrong ..."
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
        , init = init
        , update = update
        , subscriptions = always Sub.none
        , onUrlRequest = always NoOp
        , onUrlChange = always NoOp
        }
