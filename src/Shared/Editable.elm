module Shared.Editable exposing (Editable, isEditable, setEditable, setReadonly, toReadonly, toValue, updateIfEditable, valueToEditable)

import Shared.Value as Value exposing (Value)


type Editable a
    = Editable (Value a)
    | Readonly a


valueToEditable : Value a -> Editable a
valueToEditable =
    Editable


toReadonly : a -> Editable a
toReadonly =
    Readonly


setEditable : Editable a -> Editable a
setEditable editable =
    case editable of
        Readonly value ->
            Editable <| Value.validValue value

        Editable _ ->
            editable


setReadonly : Editable a -> Editable a
setReadonly editable =
    case editable of
        Editable value ->
            Readonly <| Value.fromValue value

        Readonly _ ->
            editable


toValue : Editable a -> Value a
toValue editable =
    case editable of
        Editable value ->
            value

        Readonly string ->
            Value.validValue string


updateIfEditable : a -> Editable a -> Editable a
updateIfEditable newValue editable =
    case editable of
        Editable _ ->
            Value.validValue newValue |> Editable

        Readonly value ->
            Readonly value


isEditable : Editable a -> Bool
isEditable editable =
    case editable of
        Editable _ ->
            True

        Readonly _ ->
            False
