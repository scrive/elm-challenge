module Section.Settings exposing
    ( Model
    , Msg
    , init
    , update
    , view
    )

import Data.Settings as Settings
import Html exposing (Html)


type Model
    = Model


init : Model
init =
    Model


type Msg
    = NoOp


update : Settings.Model -> Msg -> Model -> ( Model, Cmd Msg, Settings.Model )
update settings NoOp model =
    ( model
    , Cmd.none
    , settings
    )


view : Settings.Model -> Model -> Html Msg
view _ Model =
    Html.text "Settings - not implemented"
