module Main exposing (main)

import Browser
import Data
import Html exposing (Html)
import Html.Attributes as Attrs
import Json.Decode



---- MODEL ----


type Page
    = LoadingSuccess Model
    | LoadingFailed Json.Decode.Error


type alias Model =
    { userGroup : Data.UserGroup }


init : ( Page, Cmd Msg )
init =
    case Json.Decode.decodeString Data.decodeUserGroup Data.data of
        Ok userGroup ->
            ( LoadingSuccess { userGroup = userGroup }, Cmd.none )

        Err decodeError ->
            ( LoadingFailed decodeError, Cmd.none )



---- UPDATE ----


type Msg
    = NoOp


update : Msg -> Page -> ( Page, Cmd Msg )
update _ model =
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


view : Page -> Html Msg
view page =
    case page of
        LoadingSuccess model -> Html.text "Success!"
        LoadingFailed decodingError -> Html.text (Json.Decode.errorToString decodingError)



---- PROGRAM ----


main : Program () Page Msg
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
