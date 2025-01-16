module Modules.Tags exposing (..)

import Dict exposing (Dict)
import Json.Decode as Decode
import Json.Decode.Pipeline as Decode


type alias Model =
    { tags : Dict String String
    }


type alias Tag =
    { name : String
    , value : String
    }


decoder : Decode.Decoder Tag
decoder =
    Decode.succeed Tag
        |> Decode.required "name" Decode.string
        |> Decode.optional "value" Decode.string ""


initialModel : { tags : Dict String String } -> Model
initialModel { tags } =
    { tags = tags }
