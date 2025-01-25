module Scrive.Form.Field exposing
    ( BelongsTo(..)
    , Field(..)
    , belongsTo
    , toString
    )

import Scrive.Data.Address as A
import Scrive.Data.RetentionPolicy exposing (DataRetentionPolicy)


type Field
    = NewTagName
    | NewTagValue
    | NameOfTag Int
    | ValueOfTag Int
    | Address A.Field


type BelongsTo
    = Contacts
    | Tags
    | Settings


belongsTo : Field -> BelongsTo
belongsTo field =
    case field of
        NewTagName ->
            Tags

        NewTagValue ->
            Tags

        NameOfTag _ ->
            Tags

        ValueOfTag _ ->
            Tags

        Address _ ->
            Contacts


toString : Field -> String
toString f =
    case f of
        NewTagName ->
            "Name of the new tag"

        NewTagValue ->
            "Value of the new tag"

        NameOfTag n ->
            "Name of tag #" ++ String.fromInt n

        ValueOfTag n ->
            "Value of tag #" ++ String.fromInt n

        Address A.F_Email ->
            "Contacts : E-mail"

        Address A.F_Phone ->
            "Contacts : Phone"

        Address A.F_CompanyName ->
            "Contacts : Company Name"

        Address A.F_StreetAddress ->
            "Contacts : Street Address"

        Address A.F_ZipCode ->
            "Contacts : ZIP Code"

        Address A.F_City ->
            "Contacts : City"

        Address A.F_Country ->
            "Contacts : Country"
