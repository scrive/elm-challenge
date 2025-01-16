module Modules.Tags exposing (..)

import Dict exposing (Dict)
import Html exposing (Html)
import Html.Attributes as Attrs
import Html.Events as Events
import Json.Decode as Decode
import Json.Decode.Pipeline as Decode


type alias Model =
    { tags : Dict String String
    , isInherited : Bool
    }


type alias Tag =
    { name : String
    , value : String
    }


decoder : Decode.Decoder Tag
decoder =
    Decode.succeed Tag
        |> Decode.required "name" Decode.string
        |> Decode.optional "value" Decode.string ""


initialModel : { tags : Dict String String, isInherited : Bool } -> Model
initialModel { tags, isInherited } =
    { tags = tags
    , isInherited = isInherited
    }


type Msg
    = NoOp


update : Msg -> Model -> ( Model, Cmd msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )


view : Model -> Html Msg
view model =
    Html.div [ Attrs.class "w-full sm:w-auto flex flex-col justify-center border rounded p-2.5 my-2" ]
        ([ Html.h1 [ Attrs.class "text-lg text-center font-semibold text-stone-800" ] [ Html.text "Tags:" ] ]
            ++ (model.tags
                    |> Dict.toList
                    |> List.map
                        (\( name, value ) ->
                            Html.span
                                [ Attrs.class "flex flex-row py-2.5 justify-between items-center gap-4 border-b"
                                , Attrs.classList [ ( "bg-[#e8f3fc]", model.isInherited ) ]
                                ]
                                ([ Html.input
                                    [ Attrs.class "border rounded px-2 py-1 focus:outline-none border-stone-400 w-full text-md text-right text-normal text-stone-700 appearance-none"
                                    , Attrs.class "[appearance:textfield] [&::-webkit-outer-spin-button]:appearance-none [&::-webkit-inner-spin-button]:appearance-none"
                                    , Attrs.classList [ ( "bg-[#e8f3fc]", model.isInherited ) ]
                                    , Attrs.disabled model.isInherited
                                    , Attrs.value name
                                    ]
                                    []
                                 , Html.input
                                    [ Attrs.class "border rounded px-2 py-1 focus:outline-none border-stone-400 w-full text-md text-right text-normal text-stone-700 appearance-none"
                                    , Attrs.class "[appearance:textfield] [&::-webkit-outer-spin-button]:appearance-none [&::-webkit-inner-spin-button]:appearance-none"
                                    , Attrs.classList [ ( "bg-[#e8f3fc]", model.isInherited ) ]
                                    , Attrs.disabled model.isInherited
                                    , Attrs.value value
                                    ]
                                    []
                                 ]
                                    ++ (if not model.isInherited then
                                            [ Html.button
                                                [ Attrs.class "border border-transparent rounded px-2 py-1 bg-red-400 text-white outline-black hover:text-[#d2e7f9] w-12"
                                                , Events.onClick NoOp
                                                ]
                                                [ Html.text "x" ]
                                            ]

                                        else
                                            []
                                       )
                                )
                        )
               )
            ++ [ Html.span
                    [ Attrs.class "flex flex-row gap-4 my-2"
                    , Attrs.classList
                        [ ( "justify-end", not model.isInherited )
                        , ( "justify-center", model.isInherited )
                        ]
                    ]
                    (if model.isInherited then
                        [ Html.button
                            [ Attrs.class "border border-black black rounded px-2 py-1 text-black w-2/6 hover:bg-[#d2e7f9]"
                            , Attrs.type_ "button"
                            , Events.onClick NoOp
                            ]
                            [ Html.text "close" ]
                        ]

                     else
                        [ Html.button
                            [ Attrs.class "border border-black black rounded px-2 py-1 text-black hover:bg-[#d2e7f9]"
                            , Attrs.type_ "button"
                            , Events.onClick NoOp
                            ]
                            [ Html.text "cancel" ]
                        , Html.button
                            [ Attrs.class "border border-transparent rounded px-2 py-1 bg-[#1e88e2] text-white outline-black hover:text-[#d2e7f9]"
                            , Attrs.type_ "submit"
                            , Events.onClick NoOp
                            ]
                            [ Html.text "apply" ]
                        ]
                    )
               ]
        )
