module Scrive.Form.Error exposing
    ( Error
    , Field(..)
    , BelongsTo(..)
    , make
    , textOf, fieldOf, belongsTo
    , onlyBelongingTo, extractOnlyAt
    , view, viewMany
    )


import Html exposing (Html)
import Html.Extra as Html
import Html.Attributes as Attrs


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


type Error = Error ( Field, String )


make : Field -> String -> Error
make f s = Tuple.pair f s |> Error


textOf : Error -> String
textOf (Error (_, text)) = text


fieldOf : Error -> Field
fieldOf (Error (field, _)) = field


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


onlyBelongingTo : BelongsTo -> List Error -> List Error
onlyBelongingTo bt = List.filter (fieldOf >> belongsTo >> (==) bt)


extractOnlyAt : Field -> List Error -> List Error
extractOnlyAt field = List.filter (fieldOf >> (==) field)


extractOnlyAt_ : Field -> List Error -> List String
extractOnlyAt_ field = extractOnlyAt field >> List.map textOf


view : Error -> Html msg
view error = Html.span [ Attrs.class "text-red-500" ] [ Html.text <| textOf error ]


viewMany : List Error -> Html msg
viewMany errors =
    case errors of
        [] -> Html.nothing
        _ -> let
                errorItem error = Html.li [] [ view error ]
            in Html.ul [] <| List.map errorItem errors