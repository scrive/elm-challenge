module Main exposing (main)

import Browser
import Data
import UserGroup exposing (UserGroup)
import ContactDetails
import Settings
import Html exposing (Html)
import Html.Attributes as Attrs
import Json.Decode exposing (decodeString, errorToString)



---- MODEL ----


type alias Model =
    Result Json.Decode.Error
        { contactDetails : ContactDetails.State
        , settings : Settings.State
        }


init : ( Model, Cmd Msg )
init =
    ( decodeString UserGroup.decoder Data.userGroup
          |> Result.map (\userGroup ->
                             { contactDetails = ContactDetails.init userGroup.contactDetails
                             , settings = Settings.init userGroup.settings
                             }
                        )
    , Cmd.none
    )
    
    
    
---- UPDATE ----


type Msg
    = NoOp
    | ContactDetailsMsg ContactDetails.Msg
    | SettingsMsg Settings.Msg


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

        Err _ ->
            ( model, Cmd.none )



---- VIEW ----


view : Model -> Html Msg
view model =
    Html.div [ Attrs.class "mx-auto max-w-7xl px-4 py-24 sm:px-6 sm:py-32 lg:px-8" ]
        [ Html.h1
              [ Attrs.class "max-w-2xl mx-auto mt-3 text-3xl font-extrabold tracking-tight text-slate-900" ]
              [ Html.text "User group" ]
        , case model of
              Ok { contactDetails, settings } ->
                  Html.div []
                      [ ContactDetails.view contactDetails
                      |> Html.map ContactDetailsMsg
                      , Settings.view settings
                      |> Html.map SettingsMsg
                      ]

              Err error ->
                  Html.p [] [ Html.text <| errorToString error ]
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
