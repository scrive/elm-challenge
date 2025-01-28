module Main exposing (main)

import Browser
import ContactDetails exposing (PreferredContactMethod)
import Data
import DataRetentionPolicy exposing (DataRetentionPolicy)
import Html exposing (Html)
import Html.Attributes as Attrs
import Html.Events as Events
import IdleDocTimeout exposing (IdleDocTimeout(..))
import Json.Decode
import List.Extra
import Set exposing (Set)
import Svg
import Svg.Attributes
import Tag exposing (Tag)
import UserGroup exposing (Account(..), Child)



---- MODEL ----


type Model
    = Loaded LoadedModel
    | LoadingError Json.Decode.Error


type alias LoadedModel =
    { id : String
    , account : Account
    , name : String
    , children : List Child
    , inheritedFrom : Maybe String
    , idleDocTimeoutPreparation : String
    , idleDocTimeoutClosed : String
    , idleDocTimeoutCanceled : String
    , idleDocTimeoutTimedout : String
    , idleDocTimeoutRejected : String
    , idleDocTimeoutError : String
    , immediateTrash : Bool
    , contactDetailsInheritFrom : Maybe String
    , preferredContactMethod : PreferredContactMethod
    , email : String
    , phone : String
    , companyName : String
    , address : String
    , zip : String
    , city : String
    , country : String
    , visibleOptions : Set String
    , showEditOptions : Bool
    , tags : List EditableTag
    }


type EditableTag
    = NotEditing Tag
    | Editing String (Maybe Tag.Error)


currentTagNames : List EditableTag -> List String
currentTagNames editableTags =
    editableTags
        |> List.filterMap
            (\t ->
                case t of
                    Editing _ _ ->
                        Nothing

                    NotEditing tag ->
                        Just tag.name
            )


init : ( Model, Cmd Msg )
init =
    ( case Json.Decode.decodeString UserGroup.decoder Data.userGroup of
        Ok userGroup ->
            let
                dataRetentionPolicy : DataRetentionPolicy
                dataRetentionPolicy =
                    userGroup.settings.dataRetentionPolicy
            in
            Loaded
                { id = userGroup.id
                , account = userGroup.account
                , name = userGroup.name
                , children = userGroup.children
                , inheritedFrom = userGroup.settings.inheritedFrom
                , idleDocTimeoutPreparation = dataRetentionPolicy.idleDocTimeoutPreparation |> Maybe.withDefault 0 |> String.fromInt
                , idleDocTimeoutClosed = dataRetentionPolicy.idleDocTimeoutClosed |> Maybe.withDefault 0 |> String.fromInt
                , idleDocTimeoutCanceled = dataRetentionPolicy.idleDocTimeoutCanceled |> Maybe.withDefault 0 |> String.fromInt
                , idleDocTimeoutTimedout = dataRetentionPolicy.idleDocTimeoutTimedout |> Maybe.withDefault 0 |> String.fromInt
                , idleDocTimeoutRejected = dataRetentionPolicy.idleDocTimeoutRejected |> Maybe.withDefault 0 |> String.fromInt
                , idleDocTimeoutError = dataRetentionPolicy.idleDocTimeoutError |> Maybe.withDefault 0 |> String.fromInt
                , immediateTrash = dataRetentionPolicy.immediateTrash
                , contactDetailsInheritFrom = userGroup.contactDetails.inheritedFrom
                , preferredContactMethod = userGroup.contactDetails.address.preferredContactMethod
                , email = userGroup.contactDetails.address.email |> Maybe.withDefault ""
                , phone = userGroup.contactDetails.address.phone |> Maybe.withDefault ""
                , companyName = userGroup.contactDetails.address.companyName |> Maybe.withDefault ""
                , address = userGroup.contactDetails.address.address |> Maybe.withDefault ""
                , zip = userGroup.contactDetails.address.zip |> Maybe.withDefault ""
                , city = userGroup.contactDetails.address.city |> Maybe.withDefault ""
                , country = userGroup.contactDetails.address.country |> Maybe.withDefault ""
                , visibleOptions = IdleDocTimeout.visibleOptions dataRetentionPolicy
                , showEditOptions = False
                , tags = userGroup.tags |> List.map NotEditing
                }

        Err error ->
            LoadingError error
    , Cmd.none
    )



---- UPDATE ----


type Msg
    = NoOp
    | FormMsg FormMsg


type FormMsg
    = DidInputIdleDocTimeout IdleDocTimeout String
    | CheckedImmediateTrash Bool
    | ClickedEditOptions
    | ClickedDone
    | CheckOption IdleDocTimeout Bool
    | ChangePreferredContactMethod PreferredContactMethod
    | InputEmail String
    | InputPhone String
    | InputCompanyName String
    | InputAddress String
    | InputZip String
    | InputCity String
    | InputCountry String
    | AddTag
    | SaveTag Int
    | EditTag Int
    | DeleteTag Int
    | InputTag Int String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( model, msg ) of
        ( Loaded loadedModel, FormMsg formMsg ) ->
            ( Loaded (updateForm formMsg loadedModel), Cmd.none )

        _ ->
            ( model, Cmd.none )


updateForm : FormMsg -> LoadedModel -> LoadedModel
updateForm msg model =
    case msg of
        DidInputIdleDocTimeout timeout value ->
            case timeout of
                Preparation ->
                    { model | idleDocTimeoutPreparation = value }

                Closed ->
                    { model | idleDocTimeoutClosed = value }

                Canceled ->
                    { model | idleDocTimeoutCanceled = value }

                Timedout ->
                    { model | idleDocTimeoutTimedout = value }

                Rejected ->
                    { model | idleDocTimeoutRejected = value }

                Error ->
                    { model | idleDocTimeoutError = value }

        CheckedImmediateTrash checked ->
            { model | immediateTrash = checked }

        ClickedEditOptions ->
            { model | showEditOptions = True }

        ClickedDone ->
            { model | showEditOptions = False }

        CheckOption option True ->
            { model | visibleOptions = Set.insert (IdleDocTimeout.id option) model.visibleOptions }

        CheckOption option False ->
            { model | visibleOptions = Set.remove (IdleDocTimeout.id option) model.visibleOptions }

        ChangePreferredContactMethod method ->
            { model | preferredContactMethod = method }

        InputEmail email ->
            { model | email = email }

        InputPhone phone ->
            { model | phone = phone }

        InputCompanyName companyName ->
            { model | companyName = companyName }

        InputAddress address ->
            { model | address = address }

        InputZip zip ->
            { model | zip = zip }

        InputCity city ->
            { model | city = city }

        InputCountry country ->
            { model | country = country }

        AddTag ->
            { model | tags = model.tags ++ [ Editing "" Nothing ] }

        SaveTag index ->
            case List.Extra.getAt index model.tags of
                Just editableTag ->
                    case editableTag of
                        Editing tagString _ ->
                            case Tag.make (currentTagNames model.tags) tagString of
                                Ok tag ->
                                    { model | tags = List.Extra.setAt index (NotEditing tag) model.tags }

                                Err error ->
                                    { model | tags = List.Extra.setAt index (Editing tagString (Just error)) model.tags }

                        NotEditing _ ->
                            model

                Nothing ->
                    model

        EditTag index ->
            { model
                | tags =
                    List.Extra.updateAt index
                        (\editableTag ->
                            case editableTag of
                                NotEditing tag ->
                                    Editing (Tag.toString tag) Nothing

                                editing ->
                                    editing
                        )
                        model.tags
            }

        DeleteTag index ->
            { model | tags = List.Extra.removeAt index model.tags }

        InputTag index tagString ->
            { model | tags = List.Extra.setAt index (Editing tagString Nothing) model.tags }



---- VIEW ----


view : Model -> Html Msg
view model =
    case model of
        LoadingError error ->
            Html.text (Json.Decode.errorToString error)

        Loaded loadedModel ->
            loadedModelView loadedModel |> Html.map FormMsg


loadedModelView : LoadedModel -> Html FormMsg
loadedModelView model =
    Html.form [ Attrs.class "flex flex-col gap-12 mt-8 px-6 mx-auto max-w-4xl mb-8" ]
        [ section
            { title = "User group settings"
            , content = userGroupSettingsView model
            }
        , Html.div [ Attrs.class "flex flex-wrap gap-12" ]
            [ section
                { title = "Contact Details"
                , content = contactDetailsView model
                }
            , section
                { title = "Settings"
                , content = settingsView model
                }
            , section
                { title = "Tags"
                , content = tagsView model.tags
                }
            ]
        , Html.input
            [ Attrs.type_ "submit"
            , Attrs.value "Save User Group"
            , Attrs.class "p-2 bg-sky-500 rounded text-sky-50 font-medium w-40 max-w-xs self-end"
            ]
            []
        ]


userGroupSettingsView : LoadedModel -> Html FormMsg
userGroupSettingsView model =
    Html.div [ Attrs.class "flex flex-col gap-4" ]
        [ Html.div [ Attrs.class "flex flex-wrap gap-x-12 gap-y-2" ]
            [ Html.div [ Attrs.class "flex flex-col" ]
                [ Html.span [ Attrs.class "text-sm" ] [ Html.text "name" ]
                , Html.span [ Attrs.class "font-bold" ] [ Html.text model.name ]
                ]
            , Html.div [ Attrs.class "flex flex-col" ]
                [ Html.span [ Attrs.class "text-sm" ] [ Html.text "id" ]
                , Html.span [ Attrs.class "font-bold" ] [ Html.text model.id ]
                ]
            , case model.account of
                Parent parentId ->
                    Html.div [ Attrs.class "flex flex-col" ]
                        [ Html.span [ Attrs.class "text-sm" ] [ Html.text "parent id" ]
                        , Html.span [ Attrs.class "font-bold" ] [ Html.text parentId ]
                        ]

                Root ->
                    Html.span [ Attrs.class "font-bold" ] [ Html.text "root user" ]
            ]
        , case model.children of
            [] ->
                Html.text ""

            children ->
                Html.div []
                    [ Html.span [ Attrs.class "text-sm" ] [ Html.text "children" ]
                    , Html.table [ Attrs.class "text-left border-2" ]
                        (Html.tr [ Attrs.class "text-sm border-2" ]
                            [ Html.th [ Attrs.class "px-2" ] [ Html.text "id" ]
                            , Html.th [ Attrs.class "px-2" ] [ Html.text "name" ]
                            ]
                            :: List.map
                                (\child ->
                                    Html.tr []
                                        [ Html.td [ Attrs.class "px-2" ] [ Html.text child.id ]
                                        , Html.td [ Attrs.class "px-2" ] [ Html.text child.name ]
                                        ]
                                )
                                children
                        )
                    ]
        ]


settingsView : LoadedModel -> Html FormMsg
settingsView model =
    case model.inheritedFrom of
        Just from ->
            inheritedDataRetentionPolicyView from model

        Nothing ->
            dataRetentionPolicyView model


contactDetailsView : LoadedModel -> Html FormMsg
contactDetailsView model =
    case model.contactDetailsInheritFrom of
        Just parentId ->
            inheritedAddressView parentId model

        Nothing ->
            addressView model


inheritedAddressView : String -> LoadedModel -> Html msg
inheritedAddressView parentId model =
    Html.div [ Attrs.class "flex flex-col gap-8" ]
        [ Html.span [ Attrs.class "text-sm" ]
            [ Html.text ("inherited from: " ++ parentId) ]
        , Html.div [ Attrs.class "flex flex-col gap-4" ]
            [ inheritedAddressField { title = "email", value = model.email }
            , inheritedAddressField { title = "phone", value = model.phone }
            , inheritedAddressField { title = "company name", value = model.companyName }
            , inheritedAddressField { title = "address", value = model.address }
            , inheritedAddressField { title = "zip", value = model.zip }
            , inheritedAddressField { title = "city", value = model.city }
            , inheritedAddressField { title = "country", value = model.country }
            ]
        ]


inheritedAddressField : { title : String, value : String } -> Html msg
inheritedAddressField { title, value } =
    Html.div
        [ Attrs.class "flex flex-col" ]
        [ Html.span [ Attrs.class "text-sm" ] [ Html.text title ]
        , Html.span [ Attrs.class "font-bold" ]
            [ Html.text
                (if String.isEmpty value then
                    "-"

                 else
                    value
                )
            ]
        ]


addressView : LoadedModel -> Html FormMsg
addressView model =
    Html.div [ Attrs.class "flex flex-wrap gap-4" ]
        [ Html.div [ Attrs.class "flex flex-col gap-4" ]
            [ Html.div [ Attrs.class "flex flex-col gap-1" ]
                [ Html.label [ Attrs.class "text-sm" ] [ Html.text "email" ]
                , Html.input
                    [ Attrs.type_ "email"
                    , Attrs.class "border-2"
                    , Attrs.id "address-email"
                    , Attrs.name "address-email"
                    , Attrs.required (model.preferredContactMethod == ContactDetails.Email)
                    , Attrs.value model.email
                    , Events.onInput InputEmail
                    ]
                    []
                ]
            , Html.div [ Attrs.class "flex flex-col gap-1" ]
                [ Html.label [ Attrs.class "text-sm" ] [ Html.text "phone" ]
                , Html.input
                    [ Attrs.type_ "tel"
                    , Attrs.class "border-2"
                    , Attrs.id "address-phone"
                    , Attrs.name "address-phone"
                    , Attrs.placeholder "###-###-####"
                    , Attrs.pattern "[0-9]{3}-[0-9]{3}-[0-9]{4}"
                    , Attrs.required (model.preferredContactMethod == ContactDetails.Phone)
                    , Attrs.value model.phone
                    , Events.onInput InputPhone
                    ]
                    []
                ]
            , Html.div [ Attrs.class "flex flex-col gap-1" ]
                [ Html.label [ Attrs.class "text-sm" ] [ Html.text "company name" ]
                , Html.input
                    [ Attrs.class "border-2"
                    , Attrs.id "company-name"
                    , Attrs.name "company-name"
                    , Attrs.required (model.preferredContactMethod == ContactDetails.Post)
                    , Attrs.value model.companyName
                    , Events.onInput InputCompanyName
                    ]
                    []
                ]
            ]
        , Html.div [ Attrs.class "flex flex-col gap-4" ]
            [ Html.div [ Attrs.class "flex flex-col gap-1" ]
                [ Html.label [ Attrs.class "text-sm" ] [ Html.text "address" ]
                , Html.input
                    [ Attrs.class "border-2"
                    , Attrs.id "address"
                    , Attrs.name "address"
                    , Attrs.required (model.preferredContactMethod == ContactDetails.Post)
                    , Attrs.value model.address
                    , Events.onInput InputAddress
                    ]
                    []
                ]
            , Html.div [ Attrs.class "flex flex-col gap-1" ]
                [ Html.label [ Attrs.class "text-sm" ] [ Html.text "zip" ]
                , Html.input
                    [ Attrs.class "border-2"
                    , Attrs.id "zip"
                    , Attrs.name "zip"
                    , Attrs.required (model.preferredContactMethod == ContactDetails.Post)
                    , Attrs.value model.zip
                    , Events.onInput InputZip
                    ]
                    []
                ]
            , Html.div [ Attrs.class "flex flex-col gap-1" ]
                [ Html.label [ Attrs.class "text-sm" ] [ Html.text "city" ]
                , Html.input
                    [ Attrs.class "border-2"
                    , Attrs.id "city"
                    , Attrs.name "city"
                    , Attrs.required (model.preferredContactMethod == ContactDetails.Post)
                    , Attrs.value model.city
                    , Events.onInput InputCity
                    ]
                    []
                ]
            , Html.div [ Attrs.class "flex flex-col gap-1" ]
                [ Html.label [ Attrs.class "text-sm" ] [ Html.text "country" ]
                , Html.input
                    [ Attrs.class "border-2"
                    , Attrs.id "country"
                    , Attrs.name "country"
                    , Attrs.required (model.preferredContactMethod == ContactDetails.Post)
                    , Attrs.value model.country
                    , Events.onInput InputCountry
                    ]
                    []
                ]
            , preferredContactMethodView model.preferredContactMethod
            ]
        ]


preferredContactMethodView : PreferredContactMethod -> Html FormMsg
preferredContactMethodView method =
    Html.fieldset []
        [ Html.legend [ Attrs.class "text-sm" ] [ Html.text "preferred contact method" ]
        , preferredContactMethodOption
            { id = "preferred-contact-method-email"
            , title = "Email"
            , checked = method == ContactDetails.Email
            , onClick = ChangePreferredContactMethod ContactDetails.Email
            }
        , preferredContactMethodOption
            { id = "preferred-contact-method-phone"
            , title = "Phone"
            , checked = method == ContactDetails.Phone
            , onClick = ChangePreferredContactMethod ContactDetails.Phone
            }
        , preferredContactMethodOption
            { id = "preferred-contact-method-post"
            , title = "Post"
            , checked = method == ContactDetails.Post
            , onClick = ChangePreferredContactMethod ContactDetails.Post
            }
        ]


preferredContactMethodOption :
    { id : String
    , title : String
    , checked : Bool
    , onClick : FormMsg
    }
    -> Html FormMsg
preferredContactMethodOption { id, title, checked, onClick } =
    Html.div [ Attrs.class "flex gap-1" ]
        [ Html.input
            [ Attrs.type_ "radio"
            , Attrs.id id
            , Attrs.name "preferred-contact-method"
            , Attrs.value id
            , Attrs.checked checked
            , Events.onClick onClick
            ]
            []
        , Html.label [ Attrs.for id ] [ Html.text title ]
        ]


section :
    { title : String
    , content : Html FormMsg
    }
    -> Html FormMsg
section { title, content } =
    Html.section [ Attrs.class "flex flex-col gap-4" ]
        [ Html.h2
            [ Attrs.class "text-2xl font-extrabold text-transparent bg-clip-text bg-gradient-to-br from-slate-400 to-slate-800" ]
            [ Html.text title ]
        , content
        ]


inheritedDataRetentionPolicyView : String -> LoadedModel -> Html msg
inheritedDataRetentionPolicyView from model =
    let
        enabledTimeouts : List Item
        enabledTimeouts =
            filterEnabledTimeouts model
    in
    Html.div [ Attrs.class "flex flex-col gap-8" ]
        [ Html.span [ Attrs.class "text-sm" ] [ Html.text <| "inherited from: " ++ from ]
        , Html.div [ Attrs.class "flex" ] <| List.concatMap inheritedField enabledTimeouts
        ]


inheritedField : Item -> List (Html msg)
inheritedField { timeout, value } =
    [ Html.span [ Attrs.class "inline-block w-32" ] [ Html.text (IdleDocTimeout.title timeout) ]
    , Html.span [ Attrs.class "text-right w-20" ] [ Html.text value ]
    ]


dataRetentionPolicyView : LoadedModel -> Html FormMsg
dataRetentionPolicyView model =
    Html.div [ Attrs.class "w-48" ]
        [ if model.showEditOptions then
            Html.div [ Attrs.class "flex flex-col gap-1" ]
                [ Html.input
                    [ Attrs.class "self-end text-sky-500 text-sm"
                    , Attrs.type_ "button"
                    , Attrs.value "Done"
                    , Events.onClick ClickedDone
                    ]
                    []
                , Html.div [ Attrs.class "flex flex-col" ] (List.map (editOptionView model.visibleOptions) IdleDocTimeout.all)
                ]

          else
            let
                enabledTimeouts : List Item
                enabledTimeouts =
                    filterEnabledTimeouts model
            in
            Html.div [ Attrs.class "flex flex-col gap-4" ]
                [ Html.div [ Attrs.class "flex flex-col gap-2" ]
                    [ Html.input
                        [ Attrs.class "self-end text-sky-500 text-sm"
                        , Attrs.type_ "button"
                        , Attrs.value "Edit options"
                        , Events.onClick ClickedEditOptions
                        ]
                        []
                    , Html.div [ Attrs.class "flex flex-col gap-1" ] (List.map (enabledTimeoutView model.immediateTrash) enabledTimeouts)
                    ]
                , trash model.immediateTrash
                ]
        ]


editOptionView : Set String -> IdleDocTimeout -> Html FormMsg
editOptionView visibleOptions option =
    let
        id : String
        id =
            "enable_" ++ IdleDocTimeout.id option
    in
    Html.div [ Attrs.class "flex gap-2" ]
        [ Html.input
            [ Attrs.type_ "checkbox"
            , Attrs.id id
            , Attrs.checked (Set.member (IdleDocTimeout.id option) visibleOptions)
            , Events.onCheck (CheckOption option)
            ]
            []
        , Html.label [ Attrs.for id ] [ Html.text (IdleDocTimeout.title option) ]
        ]


trash : Bool -> Html FormMsg
trash immediateTrash =
    Html.p [ Attrs.class "flex gap-2" ]
        [ Html.input
            [ Attrs.type_ "checkbox"
            , Attrs.id "immediate_trash"
            , Attrs.checked immediateTrash
            , Events.onCheck CheckedImmediateTrash
            ]
            []
        , Html.label [ Attrs.for "immediate_trash" ] [ Html.text "Trash immediately" ]
        ]


enabledTimeoutView : Bool -> Item -> Html FormMsg
enabledTimeoutView trashImmediately { timeout, value } =
    Html.div
        [ Attrs.class "flex" ]
        [ Html.label
            [ Attrs.for (IdleDocTimeout.id timeout)
            , Attrs.class "inline-block w-full"
            ]
            [ Html.text (IdleDocTimeout.title timeout) ]
        , Html.input
            [ Attrs.id (IdleDocTimeout.id timeout)
            , Attrs.type_ "number"
            , Attrs.step "1"
            , Attrs.min "0"
            , Attrs.required True
            , Attrs.pattern "\\d+"
            , Attrs.value value
            , Events.onInput (DidInputIdleDocTimeout timeout)
            , Attrs.class "text-right w-20 border-2 rounded"
            , Attrs.disabled trashImmediately
            ]
            []
        ]


type alias Item =
    { timeout : IdleDocTimeout
    , value : String
    }


filterEnabledTimeouts : LoadedModel -> List Item
filterEnabledTimeouts model =
    List.filterMap identity
        [ model.idleDocTimeoutPreparation |> maybeItem Preparation model
        , model.idleDocTimeoutClosed |> maybeItem Closed model
        , model.idleDocTimeoutCanceled |> maybeItem Canceled model
        , model.idleDocTimeoutTimedout |> maybeItem Timedout model
        , model.idleDocTimeoutRejected |> maybeItem Rejected model
        , model.idleDocTimeoutError |> maybeItem Error model
        ]


maybeItem : IdleDocTimeout -> LoadedModel -> String -> Maybe Item
maybeItem option model value =
    if Set.member (IdleDocTimeout.id option) model.visibleOptions then
        Just (Item option value)

    else
        Nothing


tagsView : List EditableTag -> Html FormMsg
tagsView tags =
    Html.div [ Attrs.class "flex flex-col gap-2" ]
        [ Html.div [ Attrs.class "flex flex-wrap gap-2" ] (List.indexedMap tagView tags)
        , if
            List.any
                (\tag ->
                    case tag of
                        Editing _ _ ->
                            True

                        NotEditing _ ->
                            False
                )
                tags
          then
            Html.text ""

          else
            Html.input
                [ Attrs.type_ "button"
                , Attrs.value "Add a tag"
                , Attrs.class "self-start text-sm text-sky-500"
                , Events.onClick AddTag
                ]
                []
        ]


tagView : Int -> EditableTag -> Html FormMsg
tagView index editableTag =
    case editableTag of
        Editing tagString maybeError ->
            Html.div [ Attrs.class "flex border rounded text-sm" ]
                [ Html.input
                    [ Attrs.value tagString
                    , Attrs.placeholder "key:value"
                    , Attrs.class "px-2 w-36"
                    , Events.onInput (InputTag index)
                    ]
                    []
                , Html.div [ Attrs.class "bg-slate-100 px-1 text-slate-400" ]
                    [ Html.button
                        [ Attrs.type_ "button"
                        , Attrs.class "p-1"
                        , Events.onClick (DeleteTag index)
                        ]
                        [ xMarkSvg ]
                    , Html.button
                        [ Attrs.type_ "button"
                        , Attrs.class "p-1"
                        , Events.onClick (SaveTag index)
                        ]
                        [ checkSvg ]
                    ]
                , case maybeError of
                    Just error ->
                        Html.text (Tag.errorDescription error)

                    Nothing ->
                        Html.text ""
                ]

        NotEditing tag ->
            Html.div [ Attrs.class "flex gap-1 bg-slate-50 px-2 border rounded text-sm w-fit items-center" ]
                [ case tag.maybeValue of
                    Just value ->
                        Html.div [ Events.onClick (EditTag index), Attrs.class "flex" ]
                            [ Html.span [] [ Html.text (tag.name ++ ":\u{00A0}") ]
                            , Html.span [ Attrs.class "text-slate-500" ] [ Html.text value ]
                            ]

                    Nothing ->
                        Html.span [ Events.onClick (EditTag index) ] [ Html.text tag.name ]
                , Html.button
                    [ Attrs.type_ "button"
                    , Attrs.class "text-slate-400"
                    , Events.onClick (DeleteTag index)
                    ]
                    [ xMarkSvg ]
                ]


xMarkSvg : Svg.Svg msg
xMarkSvg =
    Svg.svg
        [ Svg.Attributes.fill "none"
        , Svg.Attributes.viewBox "0 0 24 24"
        , Svg.Attributes.strokeWidth "3"
        , Svg.Attributes.stroke "currentColor"
        , Svg.Attributes.class "size-6"
        , Svg.Attributes.width "16"
        , Svg.Attributes.height "16"
        ]
        [ Svg.path
            [ Svg.Attributes.strokeLinecap "round"
            , Svg.Attributes.strokeLinejoin "round"
            , Svg.Attributes.d "M6 18 18 6M6 6l12 12"
            ]
            []
        ]


checkSvg : Svg.Svg msg
checkSvg =
    Svg.svg
        [ Svg.Attributes.fill "none"
        , Svg.Attributes.viewBox "0 0 24 24"
        , Svg.Attributes.strokeWidth "3"
        , Svg.Attributes.stroke "currentColor"
        , Svg.Attributes.class "size-6"
        , Svg.Attributes.width "16"
        , Svg.Attributes.height "16"
        ]
        [ Svg.path
            [ Svg.Attributes.strokeLinecap "round"
            , Svg.Attributes.strokeLinejoin "round"
            , Svg.Attributes.d "m4.5 12.75 6 6 9-13.5"
            ]
            []
        ]



---- PROGRAM ----


main : Program () Model Msg
main =
    Browser.application
        { view =
            \model ->
                { title = "Scrive elm challenge task"
                , body = [ view model ]
                }
        , init = \_ _ _ -> init
        , update = update
        , subscriptions = always Sub.none
        , onUrlRequest = always NoOp
        , onUrlChange = always NoOp
        }
