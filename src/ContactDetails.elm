module ContactDetails exposing (Address, ContactDetails, PreferredContactMethod(..), decoder)

import Json.Decode exposing (Decoder)


type alias ContactDetails =
    { inheritedFrom : Maybe String
    , address : Address
    }


decoder : Decoder ContactDetails
decoder =
    Json.Decode.map2 ContactDetails
        (Json.Decode.field "inherited_from" (Json.Decode.maybe Json.Decode.string))
        (Json.Decode.field "address" addressDecoder)


type alias Address =
    { preferredContactMethod : PreferredContactMethod
    , email : Maybe String
    , phone : Maybe String
    , companyName : Maybe String
    , address : Maybe String
    , zip : Maybe String
    , city : Maybe String
    , country : Maybe String
    }


addressDecoder : Decoder Address
addressDecoder =
    Json.Decode.map8 Address
        (Json.Decode.field "preferred_contact_method" preferredContactMethodDecoder)
        (Json.Decode.field "email" (Json.Decode.maybe Json.Decode.string))
        (Json.Decode.field "phone" (Json.Decode.maybe Json.Decode.string))
        (Json.Decode.field "company_name" (Json.Decode.maybe Json.Decode.string))
        (Json.Decode.field "address" (Json.Decode.maybe Json.Decode.string))
        (Json.Decode.field "zip" (Json.Decode.maybe Json.Decode.string))
        (Json.Decode.field "city" (Json.Decode.maybe Json.Decode.string))
        (Json.Decode.field "country" (Json.Decode.maybe Json.Decode.string))


type PreferredContactMethod
    = Email
    | Phone
    | Post


preferredContactMethodDecoder : Decoder PreferredContactMethod
preferredContactMethodDecoder =
    Json.Decode.string
        |> Json.Decode.andThen
            (\string ->
                case string of
                    "email" ->
                        Json.Decode.succeed Email

                    "phone" ->
                        Json.Decode.succeed Phone

                    "post" ->
                        Json.Decode.succeed Post

                    unknown ->
                        Json.Decode.fail <| "unknown preferred contact method: " ++ unknown
            )
