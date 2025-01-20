module Data.NullableInt exposing (NullableInt, encode, decode)


import Json.Decode as D
import Json.Encode as E


type NullableInt = NullableInt (Maybe Int)


decode : D.Decoder NullableInt
decode =
    D.map
        (NullableInt
            << Maybe.andThen identity
            << Maybe.map String.toInt
        )
        <| D.nullable D.string


encode : NullableInt -> E.Value
encode (NullableInt mbInt) =
    Maybe.withDefault E.null
        <| Maybe.map (E.string << String.fromInt) mbInt