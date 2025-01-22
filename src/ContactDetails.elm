module ContactDetails exposing
    ( Model
    , Msg
    , decode
    , update
    , view
    )

import Html exposing (Html)
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline as Pipeline
import Types.InheritedFrom as InheritedFrom


type Model
    = Model ContactDetails


type alias ContactDetails =
    { inheritedFrom : InheritedFrom.Model
    , address : Address
    }


type alias Address =
    { preferredContactMethod : String
    , email : String
    , phone : Maybe String
    , companyName : String
    , address : String
    , zip : String
    , city : String
    , country : String
    }


decode : Decoder Model
decode =
    Decode.map Model decodeContactDetails


decodeContactDetails : Decoder ContactDetails
decodeContactDetails =
    Decode.succeed ContactDetails
        |> Pipeline.required "inherited_from" InheritedFrom.decode
        |> Pipeline.required "address" decodeAddress


decodeAddress : Decoder Address
decodeAddress =
    Decode.succeed Address
        |> Pipeline.required "preferred_contact_method" Decode.string
        |> Pipeline.required "email" Decode.string
        |> Pipeline.required "phone" (Decode.nullable Decode.string)
        |> Pipeline.required "company_name" Decode.string
        |> Pipeline.required "address" Decode.string
        |> Pipeline.required "zip" Decode.string
        |> Pipeline.required "city" Decode.string
        |> Pipeline.required "country" Decode.string


type Msg
    = NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update _ model =
    ( model
    , Cmd.none
    )


view : Model -> Html Msg
view _ =
    Html.text "Contact details"
