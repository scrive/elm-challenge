module Main exposing (Model)

import Accessibility.Aria as A11y
import Browser
import Data as Data
import Html exposing (Html)
import Html.Attributes as Attrs
import Json.Decode as Decode
import Set
import Settings exposing (..)
import SharedTypes exposing (..)



---- MODEL ----


type alias Model =
    { userGroup : UserGroup
    , isInputVisible : Set String
    }


init : ( Model, Cmd Msg )
init =
    case Decode.decodeString userGroupDecoder Data.userGroup of
        Ok data ->
            ( { userGroup = data
              , isInputVisible = Set.empty
              }
            , Cmd.none
            )

        Err err ->
            Debug.log (Debug.toString err)
                ( { userGroup = initialData
                  , isInputVisible = Set.empty
                  }
                , Cmd.none
                )


initialData : UserGroup
initialData =
    -- Provide initial settings data if needed
    { settingsData = initialSettingsData
    , id = ""
    , parentId = ""
    , name = ""
    , children = Nothing
    }


initialSettingsData : SettingsData
initialSettingsData =
    -- Provide initial settings data if needed
    { inheritedFrom = Nothing
    , dataRetentionPolicy = initialDataRetentionPolicy
    }


initialDataRetentionPolicy : DataRetentionPolicy
initialDataRetentionPolicy =
    -- Provide initial data retention policy if needed
    { idleDocTimeoutPreparation = Nothing
    , idleDocTimeoutClosed = Nothing
    , idleDocTimeoutCanceled = Nothing
    , idleDocTimeoutTimedout = Nothing
    , idleDocTimeoutRejected = Nothing
    , idleDocTimeoutError = Nothing
    , immediateTrash = False
    }



---- UPDATE ----


type Msg
    = NoOp
    | ShowInput String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        ShowInput inputId ->
            ( { model | isInputVisible = Set.insert inputId model.isInputVisible }, Cmd.none )



---- VIEW ----


header : String -> Html msg
header text =
    Html.div
        [ Attrs.class "p-2 text-5xl font-extrabold text-transparent bg-clip-text bg-gradient-to-br from-slate-400 to-slate-800"
        , A11y.label text
        ]
        [ Html.text text
        ]


view : Model -> Html Msg
view model =
    Html.div [ Attrs.class "flex flex-col items-center mx-auto mt-16 mb-48" ]
        [ header "Settings"
        , viewSettingsForm model.userGroup model.isInputVisible ShowInput
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
