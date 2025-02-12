module ContactDetails exposing (Address, ContactDetails, PreferredContactMethod(..), decoder)

import Json.Decode exposing (Decoder)
import Json.Decode.Pipeline


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
    Json.Decode.succeed Address
        |> Json.Decode.Pipeline.required "preferred_contact_method" preferredContactMethodDecoder
        |> Json.Decode.Pipeline.required "email" (Json.Decode.maybe Json.Decode.string)
        |> Json.Decode.Pipeline.required "phone" (Json.Decode.maybe Json.Decode.string)
        |> Json.Decode.Pipeline.required "company_name" (Json.Decode.maybe Json.Decode.string)
        |> Json.Decode.Pipeline.required "address" (Json.Decode.maybe Json.Decode.string)
        |> Json.Decode.Pipeline.required "zip" (Json.Decode.maybe Json.Decode.string)
        |> Json.Decode.Pipeline.required "city" (Json.Decode.maybe Json.Decode.string)
        |> Json.Decode.Pipeline.required "country" (Json.Decode.maybe Json.Decode.string)


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
