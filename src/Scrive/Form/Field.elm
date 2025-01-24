module Scrive.Form.Field exposing
    ( Field(..)
    , BelongsTo(..)
    , belongsTo
    )


type Field
    = NewTagName
    | NewTagValue
    | NameOfTagWith { name : String }
    | ValueOfTagWith { name : String }
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
        NameOfTagWith _ -> Tags
        ValueOfTagWith _ -> Tags
        AddressEmail -> Contacts
        AddressPhone -> Contacts
        AddressZip -> Contacts
        AddressStreet -> Contacts
        AddressCity -> Contacts
        AddressCountry -> Contacts
        AddressCompanyName -> Contacts
