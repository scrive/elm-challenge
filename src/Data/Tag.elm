module Data.Tag exposing
    ( Model
    , Tags
    , decode
    , emptyTag
    , getName
    , getValue
    , isSameTagName
    , updateName
    , updateValue
    )

import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline as Pipeline


type Model
    = Model Tag


type alias Tag =
    { name : String
    , value : String
    }


type alias Tags =
    List Model


emptyTag : Model
emptyTag =
    Model
        { name = ""
        , value = ""
        }


decode : Decoder Model
decode =
    Decode.map Model decodeTag


decodeTag : Decoder Tag
decodeTag =
    Decode.succeed Tag
        |> Pipeline.required "name" Decode.string
        |> Pipeline.optional "value" Decode.string ""


getName : Model -> String
getName (Model { name }) =
    name


getValue : Model -> String
getValue (Model { value }) =
    value


updateName : String -> Model -> Model
updateName name (Model tag) =
    Model { tag | name = name }


updateValue : String -> Model -> Model
updateValue value (Model tag) =
    Model { tag | value = value }


isSameTagName : Model -> Model -> Bool
isSameTagName (Model tagA) (Model tagB) =
    tagA.name == tagB.name
