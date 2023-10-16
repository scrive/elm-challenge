module Main exposing (main)

import Browser
import Data
import UserGroup exposing (UserGroup)
import Html exposing (Html)
import Html.Attributes as Attrs
import Json.Decode exposing (decodeString, errorToString)



---- MODEL ----


type alias Model =
    { userGroup : Result Json.Decode.Error UserGroup }


init : ( Model, Cmd Msg )
init =
    ( { userGroup = decodeString UserGroup.decoder Data.userGroup }
    , Cmd.none
    )



---- UPDATE ----


type Msg
    = NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
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


view : Model -> Html Msg
view model =
    Html.div [ Attrs.class "flex flex-col w-[1024px] items-center mx-auto mt-16 mb-48" ]
        [ header "Let's start your task"
        , subheader "Here are your data:"
        , case model.userGroup of
              Ok _ ->
                  Html.pre [ Attrs.class "my-8 py-4 px-12 text-sm bg-slate-100 font-mono shadow rounded" ] [ Html.text Data.userGroup ]

              Err error ->
                  Html.p [] [ Html.text <| errorToString error ]
        , header "Now turn them into form."
        , subheader "See README for details of the task. Good luck 🍀 "
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
