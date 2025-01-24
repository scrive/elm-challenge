module Scrive.Form.Field exposing
    ( Field(..)
    , BelongsTo(..)
    , belongsTo
    )


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
