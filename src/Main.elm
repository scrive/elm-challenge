module Main exposing (main)

import Browser
import Data
import Html exposing (Html)
import Json.Decode
import Element as E exposing (Element)
import Element.Font as F
import Ui.Theme as T
import Ui
import Validate


{-

TODO

- Debounce so errors show up after finished typing.
- Arrange validations errors such that submit button doesn't jump.
- Highlight invalid input fields and add Aria.invalid tags.
- Maybe refocus on invalid field when attempting to submit instead of
  disabling submit button.
- If submit button is to remain disabled, reconsider styling. Needs contrast,
  but looks like secondary button currently.
- Right now the value of the Inherited User field determines whether the
  contact details can be edited. Alternatively, one could add a checkbox
  to trigger the Inherited User field and choosing to inherit would feel
  more intentional.
- Add autofill attributes to e-mail and phone
- This form doesn't actually submit anything, but it did, it should have a
  success message of some sort.
- Better responsiveness.
- Maybe add cancel button to address edits?
- Run elm-format and elm-review!


NOTES

- I suppress a null values in the email and phone field by transforming
  it to a blank. This seemed sensible as it's displayed as blank, and we
  would restore the null if necessary when sending the updated data back
  to the server.
- I'm usually an abbreviation hater, but for view libraries like Html and
  elm-ui, I user them because otherwise they add so much visual noise to
  the view code.
- I could probably have used a library for the select component, but I am
  also a using-libraries-I-haven't-read-the-code-of hater, so I just wrote
  something from scratch!
- I used elm-ui here, but this is not necessarily a good fit for your
  production code, as it would possibly start having performance problems
  on heavy UIs with lots of rerendering! I just love its type safety and
  thoughtful API design.
- I only did one task! Happy to do more, but didn't want to burden you with too much
  code and thought this sample would be enough to assess the general quality.
- The initial possibillity of a data decode error is such a trite Elm ritual, so I exiled
  it to the bottom. I usually have a custom 'Program' in my applications to handle
  common errors like that!
- There are soo many opinions on what makes good accessibility! I'm curious
  what your standards are within the company.

-}



-- MODEL


type alias Model =
    { userGroup : Data.UserGroup }


init : Data.UserGroup -> ( Model, Cmd Msg )
init userGroup =
    ( { userGroup = userGroup }
    , Cmd.none
    )



-- UPDATE


type Msg
    = OnChangeUserInheritance String
    | OnChangePreferredMethod Data.ContactMethod
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

        OnChangePreferredMethod method ->
            ( updateAddress model <| \address ->
                { address | preferredContactMethod = method }
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
        [ E.width (E.maximum T.desktopWidth E.fill)
        , E.paddingXY T.space6 T.space9
        , E.centerX
        , E.spacing T.space5
        , F.size T.fontSize2
        ]
        [ E.el [ F.size T.fontSize4 ] (E.text "Contact Details")
        , E.column
            [ E.width E.fill
            , E.spacing T.space5
            ]
            [ case model.userGroup.parentId of
                Just _ ->
                    viewInheritUserInput model.userGroup.contactDetails

                Nothing ->
                    E.none
            , E.column
                [ E.width E.fill
                , E.spacing T.space4
                ]
                [ E.el [ F.size T.fontSize3 ] (E.text "Address")
                , viewContactDetails model model.userGroup.contactDetails.address
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
        , disabled = False
        }


viewContactDetails : Model -> Data.Address -> Element Msg
viewContactDetails model address =
    let isDisabled =
            case model.userGroup.contactDetails.inheritedFrom of
                Nothing ->
                    False

                Just _ ->
                    True
    in
    E.column
        [ E.width E.fill
        , E.spacing T.space5
        ]
        [ E.el [ F.color T.gray800 ] <| E.text <|
            if isDisabled then
                "Contact details can not be edited when inherited from another user."
            else
                "Please fill in your contact details."
        , Ui.select
            { label = "Preferred contact method"
            , selected = address.preferredContactMethod
            , options = [ Data.Email, Data.Phone, Data.Post ]
            , toOptionLabel = Data.contactMethodToString
            , fromOptionLabel = Data.stringToContactMethod
            , onSelect = OnChangePreferredMethod
            , id = "preferred-contact-method"
            , disabled = isDisabled
            }
        , E.wrappedRow
            [ E.width E.fill
            , E.spacing T.space5
            ]
            [ Ui.textInput
                { onChange = OnChangeEmail
                , value = address.email
                , placeholder = "name@email.com"
                , label = "E-mail"
                , disabled = isDisabled
                }
            , Ui.textInput
                { onChange = OnChangePhone
                , value = address.phone
                , placeholder = "+4612345678"
                , label = "Phone"
                , disabled = isDisabled
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
                , disabled = isDisabled
                }
            , Ui.textInput
                { onChange = OnChangeAddress
                , value = address.address
                , placeholder = "Grev Turegatan 11A"
                , label = "Address"
                , disabled = isDisabled
                }
            , Ui.textInput
                { onChange = OnChangeZipCode
                , value = address.zip
                , placeholder = "114 46"
                , label = "Zip code"
                , disabled = isDisabled
                }
            , Ui.textInput
                { onChange = OnChangeCity
                , value = address.city
                , placeholder = "Stockholm"
                , label = "City"
                , disabled = isDisabled
                }
            , Ui.textInput
                { onChange = OnChangeCountry
                , value = address.country
                , placeholder = "Sweden"
                , label = "Country"
                , disabled = isDisabled
                }
            ]
        , if isDisabled then
            E.none
          else
            case Validate.validate (validator address) address of
                Ok _ ->
                    Ui.button
                        { label = E.text "Save"
                        , onClick = Just NoOp
                        }

                Err errors ->
                    E.column
                        [ E.width E.fill
                        , E.spacing T.space5
                        ]
                        [ E.column [ E.width E.fill, E.spacing T.space3, F.color T.red700 ] (List.map E.text errors)
                        , Ui.button { label = E.text "Save", onClick = Nothing }
                        ]
        ]



-- VALIDATION


validator : Data.Address -> Validate.Validator String Data.Address
validator address =
    case address.preferredContactMethod of
        Data.Email ->
            Validate.all
                [ Validate.ifInvalidEmail .email (always "Please enter a valid e-mail.")
                , ifNotBlank address.phone (Validate.fromErrors ifInvalidPhone)
                ]

        Data.Phone ->
            Validate.all
                [ Validate.fromErrors ifInvalidPhone
                , ifNotBlank address.email (Validate.ifInvalidEmail .email (always "Please enter a valid e-mail."))
                ]

        Data.Post ->
            Validate.all
                [ ifNotBlank address.email (Validate.ifInvalidEmail .email (always "Please enter a valid e-mail."))
                , ifNotBlank address.phone (Validate.fromErrors ifInvalidPhone)
                , Validate.ifBlank .companyName "Company name must not be blank."
                , Validate.ifBlank .address "Address must not be blank."
                , Validate.ifBlank .zip "Zip code must not be blank."
                , Validate.ifBlank .city "City must not be blank."
                , Validate.ifBlank .country "Country must not be blank."
                ]


ifInvalidPhone : Data.Address -> List String
ifInvalidPhone address =
    case String.toList address.phone of
        '+' :: rest ->
            if List.all Char.isDigit rest then
                if List.length rest == 10 then [] else [ "Phone number must have exactly ten numbers." ]
            else
                [ "Phone number must be only contain numbers after the plus sign." ]

        _ ->
            [ "Phone number must begin with a plus sign." ]


ifNotBlank : String -> Validate.Validator String Data.Address -> Validate.Validator String Data.Address
ifNotBlank str someValidator =
    if isBlank str then Validate.fromErrors (always []) else someValidator



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
    Browser.document
        { view =
            \page ->
                { title = "Scrive elm challenge task"
                , body =
                    [ case page of
                        LoadingSuccess model ->
                            E.layoutWith
                                { options =
                                      [ E.focusStyle
                                          { borderColor = Nothing
                                          , backgroundColor = Nothing
                                          , shadow = Nothing
                                          }
                                      ]
                                }
                                []
                                (view model)

                        LoadingFailed decodingError ->
                            Html.text (Json.Decode.errorToString decodingError)
                    ]
                }
        , init =
            \_ ->
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
        }