module Modules.Tags exposing (..)

import Components.Input as Input
import Dict exposing (Dict)
import Html exposing (Html)
import Html.Attributes as Attrs
import Html.Events as Events
import Json.Decode as Decode
import Json.Decode.Pipeline as Decode


type alias Model =
    { tags : Dict Int { name : String, value : String }
    , newTagName : String
    , isInherited : Bool
    , newTagError : Maybe String
    , error : Dict Int String
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
    { tags =
        tags
            |> Dict.toList
            |> List.indexedMap (\index value -> ( index, { name = Tuple.first value, value = Tuple.second value } ))
            |> Dict.fromList
    , newTagName = ""
    , isInherited = isInherited
    , newTagError = Nothing
    , error = Dict.empty
    }


type Msg
    = NoOp
    | NameChanged Int { name : String }
    | ValueChanged Int { value : String }
    | Removed Int
    | TagAdded
    | NewTagNameChanged String
    | TagValidated Int


update : Msg -> Model -> ( Model, Cmd msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        NameChanged key { name } ->
            ( { model
                | tags =
                    Dict.update
                        key
                        (Maybe.map (\a -> { name = name, value = a.value }))
                        model.tags
              }
            , Cmd.none
            )

        ValueChanged key { value } ->
            ( { model
                | tags =
                    Dict.update
                        key
                        (Maybe.map (\a -> { name = a.name, value = value }))
                        model.tags
              }
            , Cmd.none
            )

        Removed key ->
            ( { model
                | tags = Dict.remove key model.tags
                , error = Dict.remove key model.error
              }
            , Cmd.none
            )

        TagAdded ->
            model.tags
                |> Dict.toList
                |> List.map (Tuple.second >> .name)
                |> List.filter ((==) model.newTagName)
                |> List.head
                |> Maybe.map
                    (\_ ->
                        ( { model | newTagError = Just "exists" }, Cmd.none )
                    )
                |> Maybe.withDefault
                    ( if String.isEmpty model.newTagName then
                        { model | newTagError = Just "empty" }

                      else if String.length model.newTagName > 32 then
                        { model | newTagError = Just "too long (32)" }

                      else
                        { model
                            | tags =
                                Dict.toList model.tags
                                    |> List.map Tuple.second
                                    |> (\newTags ->
                                            newTags
                                                ++ [ { name = model.newTagName, value = "" } ]
                                       )
                                    |> List.indexedMap (\index value -> ( index, value ))
                                    |> Dict.fromList
                            , newTagName = ""
                            , newTagError = Nothing
                        }
                    , Cmd.none
                    )

        NewTagNameChanged newTagName ->
            ( { model | newTagName = newTagName }, Cmd.none )

        TagValidated key ->
            ( validateTag key model
            , Cmd.none
            )


validateTag : Int -> Model -> Model
validateTag key model =
    let
        error : Maybe String
        error =
            Dict.get key model.tags
                |> Maybe.andThen
                    (\value ->
                        if String.isEmpty value.name then
                            Just "empty"

                        else if String.length value.name > 32 then
                            Just "too long (32)"

                        else if
                            Dict.values model.tags
                                |> List.map .name
                                |> List.filter ((==) value.name)
                                |> (\sameNames -> List.length sameNames > 1)
                        then
                            Just "exists"

                        else
                            Nothing
                    )
    in
    error
        |> Maybe.map
            (\err ->
                { model | error = Dict.insert key err model.error }
            )
        |> Maybe.withDefault { model | error = Dict.remove key model.error }


view : Model -> Html Msg
view model =
    Html.div [ Attrs.class "w-full sm:w-auto flex flex-col justify-center border rounded p-2.5 my-2" ]
        ([ Html.h1 [ Attrs.class "text-lg text-center font-semibold text-stone-800" ] [ Html.text "Tags:" ] ]
            ++ (if Dict.isEmpty model.tags then
                    [ Html.p [ Attrs.class "text-center" ] [ Html.text "no tags for now" ] ]

                else
                    model.tags
                        |> Dict.toList
                        |> List.map
                            (\( key, value ) ->
                                Html.span
                                    [ Attrs.class "flex flex-row px-2 justify-between border-b items-end"
                                    , Attrs.classList [ ( "bg-[#e8f3fc]", model.isInherited ) ]
                                    ]
                                    ([ Input.defaultConfig
                                        |> Input.withDisabled model.isInherited
                                        |> Input.withOnChange (Just (\newName -> NameChanged key { name = newName }))
                                        |> Input.withValue value.name
                                        |> Input.withLabel "name:"
                                        |> Input.withOnBlur (Just (TagValidated key))
                                        |> Input.withErrorMessage (Dict.get key model.error)
                                        |> Input.viewTextOrNumber
                                     , Input.defaultConfig
                                        |> Input.withDisabled model.isInherited
                                        |> Input.withOnChange (Just (\newValue -> ValueChanged key { value = newValue }))
                                        |> Input.withValue value.value
                                        |> Input.withLabel "value:"
                                        |> Input.viewTextOrNumber
                                     ]
                                        ++ (if not model.isInherited then
                                                [ Html.button
                                                    [ Attrs.class "border border-transparent rounded px-2 py-1 my-1 bg-red-400 text-white outline-black hover:text-[#d2e7f9] w-12"
                                                    , Events.onClick (Removed key)
                                                    ]
                                                    [ Html.text "x" ]
                                                ]

                                            else
                                                []
                                           )
                                    )
                            )
               )
            ++ (if model.isInherited then
                    []

                else
                    [ Html.span
                        [ Attrs.class "flex flex-row py-1 px-2 mt-2 justify-between items-end border-b"
                        , Attrs.classList [ ( "bg-[#e8f3fc]", model.isInherited ) ]
                        ]
                        [ Input.defaultConfig
                            |> Input.withLabel "new tag:"
                            |> Input.withDisabled model.isInherited
                            |> Input.withType "text"
                            |> Input.withOnChange (Just NewTagNameChanged)
                            |> Input.withErrorMessage model.newTagError
                            |> Input.withValue model.newTagName
                            |> Input.viewTextOrNumber
                        , Html.button
                            [ Attrs.class "border border-black black rounded px-2 my-1 py-1 bg-green-200 text-black hover:bg-[#d2e7f9]"
                            , Attrs.type_ "button"
                            , Events.onClick TagAdded
                            ]
                            [ Html.text "add" ]
                        ]
                    ]
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
                            , Attrs.classList [ ( "bg-[#1e88e2]", Dict.isEmpty model.error ), ( "bg-red-200", not (Dict.isEmpty model.error) ) ]
                            , Attrs.type_ "submit"
                            , Attrs.disabled (not (Dict.isEmpty model.error))
                            , Events.onClick NoOp
                            ]
                            [ Html.text "apply" ]
                        ]
                    )
               ]
        )
