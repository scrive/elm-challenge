module Main exposing (main)

import Browser
import Data
import Element exposing (Element, centerX, el, height, padding, px, width)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Html exposing (Html)
import Html.Attributes as Attrs
import Json.Decode as Decode
import Json.Encode as Encode
import Palette
import TaskSettings
import Types.UserGroup as UserGroup exposing (SettingsDto, decodeUserGroupDto, encodeUserGroupDto)



---- MODEL ----


type alias Model =
    { page : Page
    , data : String
    }


type Page
    = Home
    | TaskSettingsPage TaskSettings.Model


init : ( Model, Cmd Msg )
init =
    ( { data = Data.userGroup
      , page = Home
      }
    , Cmd.none
    )



---- UPDATE ----


type Msg
    = NoOp
    | EditSettings
    | TaskSettingsMsg TaskSettings.Msg
    | EditCanceled
    | SettingsUpdated SettingsDto


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( model.page, msg ) of
        ( _, NoOp ) ->
            ( model, Cmd.none )

        ( Home, EditSettings ) ->
            case Decode.decodeString decodeUserGroupDto model.data of
                Ok userGroup ->
                    ( { model | page = TaskSettingsPage (TaskSettings.init userGroup.settings) }, Cmd.none )

                Err _ ->
                    ( model, Cmd.none )

        ( TaskSettingsPage taskSettingsModel, TaskSettingsMsg taskSettingsMsg ) ->
            let
                newSettings =
                    TaskSettings.update taskSettingsMsg taskSettingsModel
            in
            ( { model | page = TaskSettingsPage newSettings }, Cmd.none )

        ( _, EditCanceled ) ->
            ( { model | page = Home }, Cmd.none )

        ( _, SettingsUpdated settings ) ->
            ( { model
                | data =
                    Decode.decodeString decodeUserGroupDto model.data
                        |> Result.map (UserGroup.withSettings settings >> encodeUserGroupDto >> Encode.encode 2)
                        |> Result.withDefault model.data
                , page = Home
              }
            , Cmd.none
            )

        _ ->
            ( model, Cmd.none )



---- VIEW ----


header : String -> Html msg
header text =
    Html.span [ Attrs.class "p-2 text-5xl font-extrabold text-transparent bg-clip-text bg-gradient-to-br from-slate-400 to-slate-800" ]
        [ Html.text text ]


subheader : String -> Html msg
subheader text =
    Html.span [ Attrs.class "p-2 text-2xl font-extrabold text-slate-800" ]
        [ Html.text text ]


view : Model -> Html Msg
view model =
    let
        editButton : Element Msg
        editButton =
            Input.button
                [ height <| px 50
                , width <| px 200
                , centerX
                , Border.rounded 7
                , Background.color Palette.primary
                , Font.color Palette.textLight
                ]
                { onPress = Just EditSettings
                , label = el [ centerX ] <| Element.text "Edit Settings"
                }
    in
    case model.page of
        Home ->
            Html.div [ Attrs.class "flex flex-col w-[1024px] items-center mx-auto mt-16 mb-48" ]
                [ header "Let's start your task"
                , subheader "Here are your data:"
                , Html.pre [ Attrs.class "my-8 py-4 px-12 text-sm bg-slate-100 font-mono shadow rounded" ]
                    [ Html.text model.data ]
                , Element.layout [ padding 10 ] editButton
                ]

        TaskSettingsPage taskSettingsModel ->
            TaskSettings.view { mapMsg = TaskSettingsMsg, onCancel = EditCanceled, onSubmit = SettingsUpdated } taskSettingsModel |> Element.layout []



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
