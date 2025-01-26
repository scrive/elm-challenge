module Common.FormField exposing
    ( FormField
    , getRaw
    , getVal
    , initFormFieldRaw
    , updateFormField
    )

import Common.Validation exposing (Codec, CodecError)


type alias FormField r v =
    { raw : r
    , val : Result CodecError v
    , codec : Codec r v
    }


getVal : FormField r v -> Result CodecError v
getVal formField =
    formField.val


getRaw : FormField r v -> r
getRaw formField =
    formField.raw


initFormField : Codec r v -> v -> FormField r v
initFormField codec val =
    { raw = codec.encode val
    , val = Ok val
    , codec = codec
    }


initFormFieldRaw : Codec r v -> r -> FormField r v
initFormFieldRaw codec raw =
    { raw = raw
    , val = codec.decode raw
    , codec = codec
    }


updateFormField : r -> FormField r v -> FormField r v
updateFormField raw formField =
    { raw = raw
    , val = formField.codec.decode raw
    , codec = formField.codec
    }
