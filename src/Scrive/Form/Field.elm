module Scrive.Form.Field exposing
    ( Field(..)
    , BelongsTo(..)
    , belongsTo
    , toString
    )


import Scrive.Data.RetentionPolicy exposing (DataRetentionPolicy)


type Field
    = NewTagName
    | NewTagValue
    | NameOfTag Int
    | ValueOfTag Int
    | AddressEmail
    | AddressPhone
    | AddressCompanyName
    | AddressStreet
    | AddressZip
    | AddressCity
    | AddressCountry


type BelongsTo
    = Contacts
    | Tags
    | Settings


belongsTo : Field -> BelongsTo
belongsTo field =
    case field of
        NewTagName -> Tags
        NewTagValue -> Tags
        NameOfTag _ -> Tags
        ValueOfTag _ -> Tags
        AddressEmail -> Contacts
        AddressPhone -> Contacts
        AddressZip -> Contacts
        AddressStreet -> Contacts
        AddressCity -> Contacts
        AddressCountry -> Contacts
        AddressCompanyName -> Contacts


toString : Field -> String
toString f =
    case f of
        NewTagName -> "Name of the new tag"
        NewTagValue -> "Value of the new tag"
        NameOfTag n -> "Name of tag #" ++ String.fromInt n
        ValueOfTag n -> "Value of tag #" ++ String.fromInt n
        AddressEmail -> "Contacts : E-mail"
        AddressPhone -> "Contacts : Phone"
        AddressCompanyName -> "Contacts : Company Name"
        AddressStreet -> "Contacts : Street Address"
        AddressZip -> "Contacts : ZIP Code"
        AddressCity -> "Contacts : City"
        AddressCountry -> "Contacts : Country"
