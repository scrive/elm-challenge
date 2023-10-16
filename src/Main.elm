module Main exposing (main)

import Browser
import Data
import UserGroup exposing (UserGroup)
import ContactDetails
import Settings
import Tags
import Html exposing (Html)
import Html.Attributes as Attrs
import Json.Decode exposing (decodeString, errorToString)



---- MODEL ----


type alias Model =
    Result Json.Decode.Error
        { name : String
        , contactDetails : ContactDetails.State
        , settings : Settings.State
        , tags : Tags.State
        }


init : ( Model, Cmd Msg )
init =
    ( decodeString UserGroup.decoder Data.userGroup
          |> Result.map (\userGroup ->
                             { name = userGroup.name
                             , contactDetails = ContactDetails.init userGroup.contactDetails
                             , settings = Settings.init userGroup.settings
                             , tags = Tags.init userGroup.tags
                             }
                        )
    , Cmd.none
    )
    
    
    
---- UPDATE ----


type Msg
    = NoOp
    | ContactDetailsMsg ContactDetails.Msg
    | SettingsMsg Settings.Msg
    | TagsMsg Tags.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case model of
        Ok state ->
            case msg of
                NoOp ->
                    ( model, Cmd.none )
                
                ContactDetailsMsg contactDetailsMsg ->
                    let
                        ( newState, cmd ) =
                            ContactDetails.update contactDetailsMsg state.contactDetails
                    in
                        ( Ok { state | contactDetails = newState }
                        , Cmd.map ContactDetailsMsg cmd
                        )

                SettingsMsg settingsMsg ->
                    let
                        ( newState, cmd ) =
                            Settings.update settingsMsg state.settings
                    in
                        ( Ok { state | settings = newState }
                        , Cmd.map SettingsMsg cmd
                        )

                TagsMsg tagsMsg ->
                    let
                        ( newState, cmd ) =
                            Tags.update tagsMsg state.tags
                    in
                        ( Ok { state | tags = newState }
                        , Cmd.map TagsMsg cmd
                        )

        Err _ ->
            ( model, Cmd.none )



---- VIEW ----


view : Model -> Html Msg
view model =
    Html.div [ Attrs.class "mx-auto max-w-7xl px-4 py-24 sm:px-6 sm:py-32 lg:px-8" ]
        [ case model of
              Ok { name, contactDetails, settings, tags } ->
                  Html.div []
                      [ headerView name
                      , ContactDetails.view contactDetails
                      |> Html.map ContactDetailsMsg
                      , Settings.view settings
                      |> Html.map SettingsMsg
                      , Tags.view tags
                      |> Html.map TagsMsg
                      ]

              Err error ->
                  Html.p [] [ Html.text <| errorToString error ]
        ]


headerView : String -> Html Msg
headerView name =
    Html.div [ Attrs.class "max-w-2xl mx-auto" ]
        [ Html.h1
              [ Attrs.class "mt-3 text-3xl font-extrabold tracking-tight text-slate-900" ]
              [ Html.text "User group" ]
        , Html.p
              [ Attrs.class "text-gray-500" ]
              [ Html.text name ]
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
