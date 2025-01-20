module Data.ContactDetails exposing (..)


type PreferredContact
    = PC_Email
    | PC_Post
    | PC_Phone


type Email = Email String


type ZipCode = ZipCode (List Int)


type Phone = Phone String -- TODO: Improve


type alias Address =
    { preferredContactMethod : PreferredContact
    , email : Email
    , phone : Phone
    , companyName : String
    , address : String
    , zip : ZipCode
    , city : String
    , country : String
    }


type ContactDetails =
    ContactDetails
        { inheritedFrom : Maybe Int
        , address : Address
        }