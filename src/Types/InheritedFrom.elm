module Types.InheritedFrom exposing (Model, decode)

import Json.Decode as Decode exposing (Decoder)


type Model
    = Model (Maybe String)


decode : Decoder Model
decode =
    Decode.map Model (Decode.maybe Decode.string)
