module Component.Tags exposing (..)

import Browser.Dom as Dom
import Data.UserGroup exposing (Tag)
import Debounce
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onBlur, onClick, onInput)
import List.Extra as List
import Maybe.Extra as Maybe
import Task
import Views.Icons exposing (trashIcon)


type alias Model =
    { currentTags : List Tag
    , newTags : List TagField
    , tagNameDebouncer : Debounce.Debounce ( Int, String )
    }


init : List Tag -> Model
init tags =
    { currentTags = tags
    , newTags = List.map toTagField tags
    , tagNameDebouncer = Debounce.init
    }


type alias TagField =
    { name : String
    , isNameValidated : Bool
    , value : String
    , isNew : Bool
    }


toTagField : Tag -> TagField
toTagField tag =
    { name = tag.name
    , isNameValidated = False
    , value = Maybe.withDefault "" tag.value
    , isNew = False
    }


fromTagField : TagField -> Tag
fromTagField tagField =
    { name = tagField.name
    , value = Just tagField.value
    }


updateTagName : String -> TagField -> TagField
updateTagName name tag =
    { tag | name = name }


updateTagValue : String -> TagField -> TagField
updateTagValue value tag =
    { tag | value = value }


updateIsNameValidated : Bool -> TagField -> TagField
updateIsNameValidated isNameValidated tag =
    { tag | isNameValidated = isNameValidated }



-- Debouncer --


debounceConfig : (Debounce.Msg -> Msg) -> Debounce.Config Msg
debounceConfig debounceMsg =
    { strategy = Debounce.later 500
    , transform = debounceMsg
    }


save : ( Int, String ) -> Cmd Msg
save s =
    Task.perform DebouncerSettled (Task.succeed s)



-- Update --


type Msg
    = NoOp
    | AddNewTagButtonClicked
    | RemovedTag Int
    | TagNameChanged Int String
    | DebounceTagName Debounce.Msg
    | DebouncerSettled ( Int, String )
    | TagValueChanged Int String
    | TagNameOnBlur Int
    | ApplyButtonClicked


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        AddNewTagButtonClicked ->
            ( { model
                | newTags =
                    model.newTags
                        ++ [ { name = ""
                             , value = ""
                             , isNew = True
                             , isNameValidated = False
                             }
                           ]
              }
            , Task.attempt (\_ -> NoOp) (Dom.focus <| String.fromInt (List.length model.newTags) ++ "-name")
            )

        RemovedTag id_ ->
            ( { model | newTags = List.removeAt id_ model.newTags }, Cmd.none )

        TagNameChanged id_ name ->
            let
                ( newDebouncer, cmd ) =
                    Debounce.push
                        (debounceConfig DebounceTagName)
                        ( id_, name )
                        model.tagNameDebouncer
            in
            ( { model
                | newTags = List.updateAt id_ (updateTagName name) model.newTags
                , tagNameDebouncer = newDebouncer
              }
            , cmd
            )

        DebounceTagName msg_ ->
            let
                ( newDebouncer, cmd ) =
                    Debounce.update
                        (debounceConfig DebounceTagName)
                        (Debounce.takeLast save)
                        msg_
                        model.tagNameDebouncer
            in
            ( { model | tagNameDebouncer = newDebouncer }
            , cmd
            )

        DebouncerSettled ( id_, value ) ->
            let
                updateTag =
                    updateTagName value
                        >> (\tag ->
                                if Maybe.isJust <| validationError model.newTags tag then
                                    updateIsNameValidated True tag

                                else
                                    updateIsNameValidated False tag
                           )
            in
            ( { model | newTags = List.updateAt id_ updateTag model.newTags }, Cmd.none )

        TagValueChanged id_ value ->
            ( { model | newTags = List.updateAt id_ (updateTagValue value) model.newTags }, Cmd.none )

        TagNameOnBlur id_ ->
            ( { model | newTags = List.updateAt id_ (updateIsNameValidated True) model.newTags }, Cmd.none )

        _ ->
            ( model, Cmd.none )



-- Validation --


validationError : List TagField -> TagField -> Maybe String
validationError tags tag =
    if not tag.isNew then
        Nothing

    else if String.isEmpty tag.name then
        Just "Name of the tag can not be empty"

    else if String.length tag.name > 32 then
        Just "Name of the tag can not be longer than 32 characters"

    else if List.count (\tag_ -> tag_.name == tag.name) tags > 1 then
        Just "This name is already in use by another tag"

    else
        Nothing


isFormValid : Model -> Bool
isFormValid model =
    List.all (validationError model.newTags >> Maybe.isNothing) model.newTags


isFormChanged : Model -> Bool
isFormChanged model =
    let
        oldForm =
            model.currentTags
                |> List.sortBy .name

        newForm =
            model.newTags
                |> List.map fromTagField
                |> List.sortBy .name
    in
    oldForm /= newForm



-- View --


tagView : List TagField -> Int -> TagField -> Html Msg
tagView tags id_ tag =
    let
        validationString =
            if tag.isNameValidated then
                validationError tags tag
                    |> Maybe.map (\s -> p [ class "mt-1 text-red-500 text-xs italic" ] [ text s ])

            else
                Nothing
    in
    tr []
        [ td [ class "p-4" ]
            [ if tag.isNew then
                div [ class "h-0 mb-12" ]
                    [ input
                        [ type_ "text"
                        , id <| String.fromInt id_ ++ "-name"
                        , value tag.name
                        , classList [ ( "border-red-500", Maybe.isJust validationString ) ]
                        , onInput <| TagNameChanged id_
                        , onBlur <| TagNameOnBlur id_
                        , autocomplete True
                        , class "shadow appearance-none border rounded w-full py-2 px-3 text-gray-700 leading-tight focus:outline-none focus:shadow-outline"
                        ]
                        []
                    , Maybe.withDefault (text "") validationString
                    ]

              else
                span [ class "h-0 mb-12 ms-3" ] [ text tag.name ]
            ]
        , td [ class "p-4" ]
            [ div [ class "h-0 mb-12" ]
                [ input
                    [ type_ "text"
                    , id <| String.fromInt id_ ++ "-value"
                    , value tag.value
                    , onInput <| TagValueChanged id_
                    , autocomplete True
                    , class "shadow appearance-none border rounded w-full py-2 px-3 text-gray-700 leading-tight focus:outline-none focus:shadow-outline"
                    ]
                    []
                ]
            ]
        , td [ class "p-4" ]
            [ div
                [ class "cursor-pointer h-0 mb-6"
                , onClick <| RemovedTag id_
                ]
                [ trashIcon ]
            ]
        ]


addTagButtonRow : List (Html Msg)
addTagButtonRow =
    [ tr [ class "border-t" ]
        [ td [ colspan 3, class "text-center" ]
            [ button
                [ class "mt-4 mb-4 bg-blue-500 text-white font-bold py-2 px-4 rounded"
                , onClick AddNewTagButtonClicked
                ]
                [ text "Add a new tag" ]
            ]
        ]
    ]


introductionText : String
introductionText =
    """This table displays all the tags for this user group where you can view, edit and remove them.
    Add a new tag by entering a name and an optional value for your tag. Names are required to be unique
    across different tags and can not be longer than 32 characters."""


view : Model -> Html Msg
view model =
    let
        isButtonDisabled =
            not (isFormChanged model) || not (isFormValid model)
    in
    div []
        [ p [ class "mb-8 text-sm" ] [ text introductionText ]
        , div [ class "w-3/4 relative flex flex-col h-full overflow-scroll text-gray-700 bg-white shadow-md rounded-lg bg-clip-border" ]
            [ table [ class "text-left table-auto min-w-max text-slate-800" ]
                [ thead []
                    [ tr [ class "text-slate-500 border-b border-slate-300 bg-slate-50" ]
                        [ th [ class "p-4 w-6/12" ] [ text "Name" ]
                        , th [ class "p-4 w-5/12" ] [ text "Value" ]
                        , th [ class "p-4 w-1/12" ] [ text "" ]
                        ]
                    ]
                , tbody [] <| List.indexedMap (tagView model.newTags) model.newTags ++ addTagButtonRow
                ]
            ]
        , button
            [ class "mt-4 mb-4 bg-blue-500 text-white font-bold py-2 px-4 rounded"
            , classList [ ( "opacity-50 cursor-not-allowed", isButtonDisabled ) ]
            , disabled isButtonDisabled
            , onClick ApplyButtonClicked
            ]
            [ text "Apply changes" ]
        ]
