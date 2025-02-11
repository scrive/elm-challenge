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
import Validate



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
    | OnChangePhone String
    | OnChangeCompanyName String
    | OnChangeAddress String
    | OnChangeZipCode String
    | OnChangeCity String
    | OnChangeCountry String
    | NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        OnChangeUserInheritance str ->
            ( updateContactDetails model <| \contactDetails ->
                { contactDetails | inheritedFrom = if isBlank str then Nothing else Just str }
            , Cmd.none
            )

        OnChangeEmail str ->
            ( updateAddress model <| \address ->
                { address | email = str }
            , Cmd.none
            )

        OnChangePhone str ->
            ( updateAddress model <| \address ->
                { address | phone = str }
            , Cmd.none
            )

        OnChangeCompanyName str ->
            ( updateAddress model <| \address ->
                { address | companyName = str }
            , Cmd.none
            )

        OnChangeAddress str ->
            ( updateAddress model <| \address ->
                { address | address = str }
            , Cmd.none
            )

        OnChangeZipCode str ->
            ( updateAddress model <| \address ->
                { address | zip = str }
            , Cmd.none
            )

        OnChangeCity str ->
            ( updateAddress model <| \address ->
                { address | city = str }
            , Cmd.none
            )

        OnChangeCountry str ->
            ( updateAddress model <| \address ->
                { address | country = str }
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



-- VIEW


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
                        viewContactDetailsEditable model.userGroup.contactDetails.address

                    Just _ ->
                        viewContactDetails model.userGroup.contactDetails.address
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


viewContactDetailsEditable : Data.Address -> Element Msg
viewContactDetailsEditable address =
    E.column
        [ E.width E.fill
        , E.spacing T.space5
        ]
        [ E.wrappedRow
            [ E.width E.fill
            , E.spacing T.space5
            ]
            [ Ui.textInput
                { onChange = OnChangeEmail
                , value = address.email
                , placeholder = "name@email.com"
                , label = "E-mail"
                }
            , Ui.textInput
                { onChange = OnChangePhone
                , value = address.phone
                , placeholder = "+4612345678"
                , label = "Phone"
                }
            ]
        , E.wrappedRow
            [ E.width E.fill
            , E.spacing T.space5
            ]
            [ Ui.textInput
                { onChange = OnChangeCompanyName
                , value = address.companyName
                , placeholder = "Scrive"
                , label = "Company Name"
                }
            , Ui.textInput
                { onChange = OnChangeAddress
                , value = address.address
                , placeholder = "Address"
                , label = "Address"
                }
            , Ui.textInput
                { onChange = OnChangeZipCode
                , value = address.zip
                , placeholder = "Zip code"
                , label = "Zip code"
                }
            , Ui.textInput
                { onChange = OnChangeCity
                , value = address.city
                , placeholder = "Stockholm"
                , label = "City"
                }
            , Ui.textInput
                { onChange = OnChangeCountry
                , value = address.country
                , placeholder = "Sweden"
                , label = "Country"
                }
            ]
        , case Validate.validate (validator address) address of
            Ok _ -> E.text "Ok button"
            Err errors -> E.column [ E.width E.fill ] (List.map E.text errors)
        ]


viewContactDetails : Data.Address -> Element Msg
viewContactDetails { email, phone, companyName, address, zip, city, country } =
    E.column
        [ E.width E.fill
        , E.spacing T.space5
        ]
        [ E.wrappedRow
            [ E.width E.fill
            , E.spacing T.space5
            ]
            [ Ui.textInputDisabled "E-mail" email
            , Ui.textInputDisabled "Phone" phone
            ]
        , E.wrappedRow
            [ E.width E.fill
            , E.spacing T.space5
            ]
            [ Ui.textInputDisabled "Company Name" companyName
            , Ui.textInputDisabled "Address" address
            , Ui.textInputDisabled "Zip code" zip
            , Ui.textInputDisabled "City" city
            , Ui.textInputDisabled "Country" country
            ]
        ]



-- VALIDATION


validator : Data.Address -> Validate.Validator String Data.Address
validator address =
    case address.preferredContactMethod of
        Data.Email ->
            Validate.all
                [ Validate.ifInvalidEmail .email (\_ -> "Please enter a valid e-mail.") ]

        Data.Phone ->
            Validate.all
                [ Validate.ifBlank .phone "Please enter a valid phone." ]

        Data.Post ->
            Validate.all
                [ Validate.ifBlank .companyName "Company name must not be blank."
                , Validate.ifBlank .address "Address must not be blank."
                , Validate.ifBlank .zip "Zip code must not be blank."
                , Validate.ifBlank .city "City must not be blank."
                , Validate.ifBlank .country "Country must not be blank."
                ]



-- HELPERS


isBlank : String -> Bool
isBlank str =
    String.isEmpty (String.trim str)



-- PROGRAM


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