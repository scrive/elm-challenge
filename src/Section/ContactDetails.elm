module Section.ContactDetails exposing
    ( Model
    , Msg
    , init
    , update
    , view
    )

import Data.ContactDetails as ContactDetails
import Html exposing (Html)


type Model
    = Model


init : Model
init =
    Model


type Msg
    = NoOp


update : ContactDetails.Model -> Msg -> Model -> ( Model, Cmd Msg, ContactDetails.Model )
update contactDetails NoOp model =
    ( model
    , Cmd.none
    , contactDetails
    )


view : ContactDetails.Model -> Model -> Html Msg
view _ Model =
    Html.text "Contact details - not implemented"
