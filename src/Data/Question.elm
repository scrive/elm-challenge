module Data.Question exposing
    ( Condition(..)
    , Question
    , QuestionTag(..)
    , address
    , addressQuestions
    , city
    , companyName
    , country
    , email
    , phone
    , preferredContactMethod
    , tagToString
    , validate
    , zip
    )

import Data.Answer as Answer exposing (Answer, AnswerValidator(..))
import Data.Field as Field exposing (Field, Option)
import Json.Decode exposing (Error(..))
import Utils exposing (onlyIf)
import Validate exposing (Valid)


type alias Question =
    { tag : QuestionTag
    , title : String
    , placeholder : String
    , field : Field
    , validation : Validation
    , visibility : Condition
    , interactability : Condition
    }


type QuestionTag
    = Email
    | Phone
    | PreferredContactMethod
    | CompanyName
    | Address
    | Zip
    | City
    | Country
    | IsInherited
    | InheritedFrom


type Validation
    = MustAnswer (List AnswerValidator)
    | Optional (List AnswerValidator)


type Condition
    = Unconditional
    | Conditional ( QuestionTag, Answer -> Bool )


tagToString : QuestionTag -> String
tagToString tag =
    case tag of
        Email ->
            "email"

        Phone ->
            "phone"

        PreferredContactMethod ->
            "preferredContactMethod"

        CompanyName ->
            "companyName"

        Address ->
            "address"

        Zip ->
            "zip"

        City ->
            "city"

        Country ->
            "country"

        InheritedFrom ->
            "inheritedFrom"

        IsInherited ->
            "isInherited"


email : Question
email =
    { tag = Email
    , title = "E-Mail Address"
    , placeholder = "E-Mail Address"
    , field = Field.Text Field.Email
    , validation = Optional [ ValidEmail ]
    , visibility = Unconditional
    , interactability = Conditional ( IsInherited, Answer.isEmpty )
    }


phone : Question
phone =
    { tag = Phone
    , title = "Phone Number"
    , placeholder = "Phone Number"
    , field = Field.Text Field.Phone
    , validation = Optional [ ValidPhone ]
    , visibility = Unconditional
    , interactability = Conditional ( IsInherited, Answer.isEmpty )
    }


preferredContactMethod : Question
preferredContactMethod =
    { tag = PreferredContactMethod
    , title = "Preferred Contact Method"
    , placeholder = ""
    , field = Field.Radio contactMethodOptions
    , validation = MustAnswer []
    , visibility = Unconditional
    , interactability = Conditional ( IsInherited, Answer.isEmpty )
    }


contactMethodOptions : List Option
contactMethodOptions =
    [ ( "email", "E-Mail" )
    , ( "phone", "Phone" )
    , ( "post", "Post" )
    ]


companyName : Question
companyName =
    { tag = CompanyName
    , title = "Company Name"
    , placeholder = "Company Name"
    , field = Field.Text Field.Plain
    , validation = MustAnswer [ MinLength 2 ]
    , visibility = Unconditional
    , interactability = Conditional ( IsInherited, Answer.isEmpty )
    }


address : Question
address =
    { tag = Address
    , title = "Address"
    , placeholder = "Address"
    , field = Field.Text Field.Plain
    , validation = MustAnswer [ MinLength 2 ]
    , visibility = Unconditional
    , interactability = Conditional ( IsInherited, Answer.isEmpty )
    }


zip : Question
zip =
    { tag = Zip
    , title = "Zip Code"
    , placeholder = "Zip Code"
    , field = Field.Text Field.Plain
    , validation = MustAnswer [ MinLength 4 ]
    , visibility = Unconditional
    , interactability = Conditional ( IsInherited, Answer.isEmpty )
    }


city : Question
city =
    { tag = City
    , title = "City"
    , placeholder = "City"
    , field = Field.Text Field.Plain
    , validation = MustAnswer [ MinLength 2 ]
    , visibility = Unconditional
    , interactability = Conditional ( IsInherited, Answer.isEmpty )
    }


country : Question
country =
    { tag = Country
    , title = "Country"
    , placeholder = "Country"
    , field = Field.Text Field.Plain
    , validation = MustAnswer [ MinLength 2 ]
    , visibility = Unconditional
    , interactability = Conditional ( IsInherited, Answer.isEmpty )
    }


isInherited : Question
isInherited =
    { tag = IsInherited
    , title = "Is Inherited?"
    , placeholder = ""
    , field = Field.Checkbox
    , validation = Optional []
    , visibility = Unconditional
    , interactability = Unconditional
    }


inheritedFrom : Question
inheritedFrom =
    { tag = InheritedFrom
    , title = "Inherited From"
    , placeholder = ""
    , field = Field.Text Field.Plain
    , validation = Optional []
    , visibility = Conditional ( IsInherited, Answer.isEmpty >> not )
    , interactability = Conditional ( IsInherited, Answer.isEmpty >> not )
    }


addressQuestions : List Question
addressQuestions =
    [ isInherited
    , inheritedFrom
    , companyName
    , address
    , zip
    , city
    , country
    , email
    , phone
    , preferredContactMethod
    ]


validate : Question -> Answer -> Result (List String) (Valid Answer)
validate question answer =
    Validate.validate
        (Validate.firstError
            (case question.validation of
                MustAnswer validatorTypes ->
                    List.concatMap Answer.toValidators
                        (EmptyInput :: validatorTypes)

                Optional validatorTypes ->
                    onlyIf (Answer.isEmpty answer |> not)
                        (List.concatMap Answer.toValidators validatorTypes)
            )
        )
        answer
