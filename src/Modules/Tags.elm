module Modules.Tags exposing
    ( Model
    , Msg
    , decoder
    , initialModel
    , update
    , view
    )

import Browser.Dom as Dom
import Components.Input as Input
import Dict exposing (Dict)
import Html exposing (Html)
import Html.Attributes as Attrs
import Html.Events as Events
import Json.Decode as Decode
import Json.Decode.Pipeline as Decode
import Task


type alias Model =
    { tags : Dict Int Tag
    , newTagName : String
    , newTagValue : String
    , isInherited : Bool
    , newTagError : String
    , tagsErrors : Dict Int String
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
            |> List.indexedMap
                (\index value ->
                    ( index
                    , { name = Tuple.first value
                      , value = Tuple.second value
                      }
                    )
                )
            |> Dict.fromList
    , newTagName = ""
    , newTagValue = ""
    , isInherited = isInherited
    , newTagError = ""
    , tagsErrors = Dict.empty
    }


type Msg
    = NoOp
    | NameChanged Int { name : String }
    | ValueChanged Int { value : String }
    | Removed Int
    | TagAdded
    | NewTagNameChanged String
    | NewTagValueChanged String
    | TagsValidated
    | Submitted
    | Closed


update : Msg -> Model -> { onSubmit : Model -> msg, onClose : msg, onFocus : msg } -> ( Model, Cmd msg )
update msg model config =
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
                , tagsErrors = Dict.remove key model.tagsErrors
              }
            , Cmd.none
            )

        TagAdded ->
            ( checkTag model.tags
                { name = model.newTagName
                , value = model.newTagValue
                }
                |> Maybe.map
                    (\error ->
                        { model | newTagError = error }
                    )
                |> Maybe.withDefault
                    { model
                        | tags =
                            Dict.toList model.tags
                                |> List.map Tuple.second
                                |> (\newTags ->
                                        newTags
                                            ++ [ { name = model.newTagName, value = model.newTagValue } ]
                                   )
                                |> List.indexedMap (\index value -> ( index, value ))
                                |> Dict.fromList
                        , newTagName = ""
                        , newTagValue = ""
                        , newTagError = ""
                    }
            , Task.attempt (\_ -> config.onFocus) (Dom.focus "new-tag-input")
            )

        NewTagNameChanged newTagName ->
            ( { model | newTagName = newTagName }, Cmd.none )

        NewTagValueChanged newTagValue ->
            ( { model | newTagValue = newTagValue }, Cmd.none )

        TagsValidated ->
            let
                tagsErrors : Dict Int String
                tagsErrors =
                    Dict.foldl
                        (\k _ acc ->
                            Dict.get k model.tags
                                |> Maybe.andThen (checkTag (Dict.remove k model.tags))
                                |> Maybe.map (\err -> Dict.insert k err acc)
                                |> Maybe.withDefault acc
                        )
                        Dict.empty
                        model.tags
            in
            ( { model | tagsErrors = tagsErrors }
            , Cmd.none
            )

        Submitted ->
            ( model
            , Task.perform config.onSubmit (Task.succeed model)
            )

        Closed ->
            ( model
            , Task.perform (\_ -> config.onClose) (Task.succeed "")
            )


checkTag : Dict Int Tag -> Tag -> Maybe String
checkTag tags tag =
    if String.isEmpty tag.name then
        Just "empty"

    else if String.length tag.name > 32 then
        Just "too long (32)"

    else if
        Dict.values tags
            |> List.map .name
            |> List.any ((==) tag.name)
    then
        Just "exists"

    else
        Nothing


view : Model -> Html Msg
view model =
    Html.div
        [ Attrs.class "w-full sm:w-auto flex flex-col gap-1"
        , Attrs.class "justify-center border rounded p-2.5 my-2"
        ]
        ([ Html.h1
            [ Attrs.class "text-lg text-center font-semibold text-stone-800" ]
            [ Html.text "Tags:" ]
         ]
            ++ (if Dict.isEmpty model.tags then
                    [ Html.p
                        [ Attrs.class "text-center" ]
                        [ Html.text "no tags for now" ]
                    ]

                else
                    model.tags
                        |> Dict.toList
                        |> List.map (viewTag model)
               )
            ++ (if model.isInherited then
                    []

                else
                    [ viewAddTag model ]
               )
            ++ [ viewFormSubmitSection model ]
        )


viewFormSubmitSection : Model -> Html Msg
viewFormSubmitSection model =
    Html.span
        [ Attrs.class "flex flex-row gap-4 my-2"
        , Attrs.classList
            [ ( "justify-end", not model.isInherited )
            , ( "justify-center", model.isInherited )
            ]
        ]
        (if model.isInherited then
            [ Html.button
                [ Attrs.class "w-2/6"
                , Attrs.class "border border-black rounded px-2 py-1 text-black hover:bg-[#d2e7f9]"
                , Attrs.type_ "button"
                , Events.onClick Closed
                ]
                [ Html.text "close" ]
            ]

         else
            [ Html.button
                [ Attrs.class "border border-black rounded px-2 py-1 text-black hover:bg-[#d2e7f9]"
                , Attrs.type_ "button"
                , Events.onClick Closed
                ]
                [ Html.text "cancel" ]
            , Html.button
                [ Attrs.class "border border-transparent rounded px-2 py-1 bg-[#1e88e2]"
                , Attrs.class "text-white outline-black hover:text-[#d2e7f9]"
                , Attrs.classList
                    [ ( "bg-[#1e88e2]", Dict.isEmpty model.tagsErrors )
                    , ( "bg-red-200", not (Dict.isEmpty model.tagsErrors) )
                    ]
                , Attrs.type_ "submit"
                , Attrs.disabled (not (Dict.isEmpty model.tagsErrors))
                , Events.onClick Submitted
                ]
                [ Html.text "apply" ]
            ]
        )


viewAddTag : Model -> Html Msg
viewAddTag model =
    Html.form
        [ Attrs.class "flex flex-row py-1 px-2 mt-2 justify-between items-end border-b"
        , Attrs.classList [ ( "bg-[#e8f3fc]", model.isInherited ) ]
        , Events.onSubmit TagAdded
        ]
        [ Input.defaultConfig
            |> Input.withLabel "new tag:"
            |> Input.withId "new-tag-input"
            |> Input.withDisabled model.isInherited
            |> Input.withType "text"
            |> Input.withOnChange (Just NewTagNameChanged)
            |> Input.withErrorMessage model.newTagError
            |> Input.withValue model.newTagName
            |> Input.viewTextOrNumber
        , Input.defaultConfig
            |> Input.withLabel "value:"
            |> Input.withId "new-tag-value-input"
            |> Input.withDisabled model.isInherited
            |> Input.withOnChange (Just NewTagValueChanged)
            |> Input.withValue model.newTagValue
            |> Input.viewTextOrNumber
        , Html.input [ Attrs.type_ "submit", Attrs.class "hidden" ]
            []
        , Html.button
            [ Attrs.class "my-1 bg-green-200"
            , Attrs.class "border border-black rounded px-2 py-1 text-black hover:bg-[#d2e7f9]"
            , Attrs.type_ "button"
            , Events.onClick TagAdded
            ]
            [ Html.text "add" ]
        ]


viewTag : Model -> ( Int, { name : String, value : String } ) -> Html Msg
viewTag model ( key, value ) =
    Html.span
        [ Attrs.class "flex flex-row px-2 justify-between border-b items-end rounded"
        , Attrs.classList [ ( "bg-[#e8f3fc]", model.isInherited ) ]
        ]
        ([ Input.defaultConfig
            |> Input.withDisabled model.isInherited
            |> Input.withOnChange (Just (\newName -> NameChanged key { name = newName }))
            |> Input.withValue value.name
            |> Input.withLabel "name:"
            |> Input.withOnBlur (Just TagsValidated)
            |> Input.withErrorMessage (Dict.get key model.tagsErrors |> Maybe.withDefault "")
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
                        [ Attrs.class "border border-transparent rounded px-2 py-1 my-1 bg-red-400"
                        , Attrs.class "text-white outline-black hover:text-[#d2e7f9] w-12"
                        , Events.onClick (Removed key)
                        ]
                        [ Html.text "x" ]
                    ]

                else
                    []
               )
        )
