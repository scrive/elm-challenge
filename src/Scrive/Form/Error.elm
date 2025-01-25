module Scrive.Form.Error exposing
    ( Error
    , extractOnlyAt
    , fieldOf
    , make
    , onlyBelongingTo
    , textOf
    , view
    , viewMany
    )

import Html exposing (Html)
import Html.Attributes as Attrs
import Html.Extra as Html
import Scrive.Form.Field as Field exposing (Field)
import Style as Style


type Error
    = Error ( Field, String )


make : Field -> String -> Error
make f s =
    Tuple.pair f s |> Error


textOf : Error -> String
textOf (Error ( _, text )) =
    text


fieldOf : Error -> Field
fieldOf (Error ( field, _ )) =
    field


onlyBelongingTo : Field.BelongsTo -> List Error -> List Error
onlyBelongingTo bt =
    List.filter (fieldOf >> Field.belongsTo >> (==) bt)


extractOnlyAt : Field -> List Error -> List Error
extractOnlyAt field =
    List.filter (fieldOf >> (==) field)


view : Error -> Html msg
view error =
    Html.span [ Attrs.class "text-red-500" ] [ Html.text <| textOf error ]


viewMany : List Error -> Html msg
viewMany errors =
    case errors of
        [] ->
            Html.nothing

        _ ->
            let
                errorItem error =
                    Html.li [ Attrs.class Style.errorItem ] [ view error ]
            in
            Html.ul [ Attrs.class Style.errorsList ] <| List.map errorItem errors
