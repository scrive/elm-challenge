module Main exposing (main)

import Browser
import Data
import Html exposing (Html)
import Html.Attributes as Attrs
import Json.Decode
import Element as E exposing (Element)
import Element.Input as I
import Element.Font as F
import Element.Border as B
import Element.Events as EE
import Element.Background as BG
import Ui.Theme as T
import Ui



---- MODEL ----


type alias Model =
    { userGroup : Data.UserGroup }


init : Data.UserGroup -> ( Model, Cmd Msg )
init userGroup =
    ( { userGroup = userGroup }, Cmd.none )



---- UPDATE ----


type Msg
    = OnChangeUserInheritance String
    | OnChangeEmail String
    | NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        OnChangeUserInheritance str ->
            ( updateContactDetails model <| \contactDetails ->
                { contactDetails | inheritedFrom = if String.isEmpty (String.trim str) then Nothing else Just str }
            , Cmd.none
            )

        OnChangeEmail str ->
            ( updateAddress model <| \address ->
                { address | email = if String.isEmpty (String.trim str) then Nothing else Just str }
            , Cmd.none
            )

        NoOp ->
            ( model, Cmd.none )


updateContactDetails : Model -> (Data.ContactDetails -> Data.ContactDetails) -> Model
updateContactDetails ({ userGroup } as model) func =
    { model | userGroup = { userGroup | contactDetails = func userGroup.contactDetails } }


updateAddress : Model -> (Data.Address -> Data.Address) -> Model
updateAddress ({ userGroup } as model) func =
    let contactDetails = userGroup.contactDetails in
    { model | userGroup = { userGroup | contactDetails = { contactDetails | address = func contactDetails.address } } }



---- VIEW ----


view : Model -> Element Msg
view model =
    E.column
        [ E.width (E.px T.desktopWidth)
        , E.paddingXY T.space3 T.space10
        , E.centerX
        , E.spacing T.space5
        , F.size T.fontSize2
        ]
        [ E.el [ F.size T.fontSize4 ] (E.text "Contact Details")
        , E.column
            [ E.width E.fill
            , E.spacing T.space5
            ]
            [ viewInheritUserInput model.userGroup.contactDetails

            , E.column
                [ E.width E.fill
                , E.spacing T.space4
                ]
                [ E.el [ F.size T.fontSize3 ] (E.text "Address")
                , case model.userGroup.contactDetails.inheritedFrom of
                    Nothing ->
                        viewContactDetailsEditable model.userGroup.contactDetails

                    Just _ ->
                        viewContactDetails model.userGroup.contactDetails
                ]
            ]
        ]


viewInheritUserInput : Data.ContactDetails -> Element Msg
viewInheritUserInput contactDetails =
    Ui.textInput
        { onChange = OnChangeUserInheritance
        , value = Maybe.withDefault "" contactDetails.inheritedFrom
        , placeholder = "User id"
        , label = "Inherit from user"
        }


viewContactDetailsEditable : Data.ContactDetails -> Element Msg
viewContactDetailsEditable contactDetails =
    E.column
        [ E.width E.fill
        , E.spacing T.space5
        ]
        [ Ui.textInput
            { onChange = OnChangeEmail
            , value = Maybe.withDefault "" contactDetails.address.email
            , placeholder = "name@email.com"
            , label = "E-mail"
            }
        ]


viewContactDetails : Data.ContactDetails -> Element Msg
viewContactDetails contactDetails =
    E.column
        [ E.width E.fill
        , E.spacing T.space5
        ]
        [ E.el [ F.color T.gray600 ] (E.text "Inherited contact details are not editable.")
        , Ui.textInputDisabled "E-mail" (Maybe.withDefault "" contactDetails.address.email)
        ]



---- PROGRAM ----


type Page
    = LoadingSuccess Model
    | LoadingFailed Json.Decode.Error


main : Program () Page Msg
main =
    Browser.application
        { view =
            \page ->
                { title = "Scrive elm challenge task"
                , body =
                    [ case page of
                        LoadingSuccess model ->
                            E.layout [] (view model)

                        LoadingFailed decodingError ->
                            Html.text (Json.Decode.errorToString decodingError)
                    ]
                }
        , init =
            \_ _ _ ->
                case Json.Decode.decodeString Data.decodeUserGroup Data.data of
                    Ok userGroup ->
                        init userGroup
                            |> Tuple.mapFirst LoadingSuccess

                    Err decodeError ->
                        ( LoadingFailed decodeError, Cmd.none )
        , update =
            \msg page ->
                case page of
                    LoadingSuccess model ->
                        update msg model
                            |> Tuple.mapFirst LoadingSuccess

                    LoadingFailed _ ->
                        ( page, Cmd.none )
        , subscriptions = always Sub.none
        , onUrlRequest = always NoOp
        , onUrlChange = always NoOp
        }
