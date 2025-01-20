module Form.Tags exposing (Model, Msg, decode, update, view)

import Html exposing (Html)
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline as Pipeline


type Model
    = Model (List Tag)


type alias Tag =
    { name : String
    , value : String
    }


decode : Decoder Model
decode =
    Decode.map Model (Decode.list decodeTag)


decodeTag : Decoder Tag
decodeTag =
    Decode.succeed Tag
        |> Pipeline.required "name" Decode.string
        |> Pipeline.optional "value" Decode.string ""


type Msg
    = NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    ( model
    , Cmd.none
    )


view : Model -> Html Msg
view (Model model) =
    Html.text "Tags"
