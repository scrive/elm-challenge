module Util.Validate exposing
    ( Errors
    , validateTagName
    , validateTagValue
    )

import Data.Tag as Tag


type alias Errors =
    List String


validateTagValue : Tag.Model -> Tag.Tags -> Errors
validateTagValue _ _ =
    []


validateTagName : Bool -> Tag.Model -> Tag.Tags -> Errors
validateTagName skipDuplicityValidation tag tags =
    [ getTagNameRequiredError tag
    , getMaxTagNameLengthError tag
    , if skipDuplicityValidation then
        ""

      else
        getDuplicityTagError tag tags
    ]
        |> List.filter (String.isEmpty >> not)


maxTagNameLength : Int
maxTagNameLength =
    32


getTagNameRequiredError : Tag.Model -> String
getTagNameRequiredError tag =
    let
        tagName =
            Tag.getName tag
    in
    if String.isEmpty tagName then
        "Value is required"

    else
        ""


getMaxTagNameLengthError : Tag.Model -> String
getMaxTagNameLengthError tag =
    let
        length =
            tag
                |> Tag.getName
                |> String.length
    in
    if length > maxTagNameLength then
        "Max allowed characters amount is " ++ String.fromInt maxTagNameLength

    else
        ""


getDuplicityTagError : Tag.Model -> Tag.Tags -> String
getDuplicityTagError tag tags =
    let
        isDuplicity =
            tags
                |> List.filter (Tag.isSameTagName tag)
                |> List.isEmpty
                |> not
    in
    if isDuplicity then
        "Tag with same name already exists"

    else
        ""
