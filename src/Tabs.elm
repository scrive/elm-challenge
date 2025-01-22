module Tabs exposing
    ( Model
    , Msg
    , Tab(..)
    , init
    , update
    , view
    )

import Html exposing (Html)
import Html.Attributes as Attributes
import Html.Events as Events


type alias Model =
    Tab


type Tab
    = ContactDetails
    | Settings
    | Tags


init : Model
init =
    Tags


type Msg
    = ChangedTab Tab


update : Msg -> Model -> ( Model, Cmd Msg )
update (ChangedTab tab) _ =
    ( tab
    , Cmd.none
    )


orderedTabs : List Tab
orderedTabs =
    [ ContactDetails
    , Settings
    , Tags
    ]


wrapperCssClassNames : String
wrapperCssClassNames =
    "text-sm font-medium text-center text-gray-500 border-b border-gray-200 dark:text-gray-400 dark:border-gray-700"


view : Model -> Html Msg
view model =
    Html.div [ Attributes.class wrapperCssClassNames ]
        [ model
            |> tabsView
            |> Html.ul [ Attributes.class "flex flex-wrap -mb-px" ]
        ]


tabsView : Model -> List (Html Msg)
tabsView model =
    orderedTabs
        |> List.indexedMap (tabView model)


baseTabCssClasses : String
baseTabCssClasses =
    "inline-block p-4 border-b-2 rounded-t-lg cursor-pointer"


tabCssClasses : String
tabCssClasses =
    "border-transparent hover:text-gray-600 hover:border-gray-300 dark:hover:text-gray-300"


tabActiveCssClasses : String
tabActiveCssClasses =
    "text-blue-600 border-blue-600 active dark:text-blue-500 dark:border-blue-500"


tabView : Model -> Int -> Tab -> Html Msg
tabView activeTab index tab =
    let
        tabStateClass =
            if activeTab == tab then
                tabActiveCssClasses

            else
                tabCssClasses

        isNotLast =
            index + 1 /= List.length orderedTabs

        wrapperAttributes =
            if isNotLast then
                [ Attributes.class "me-2" ]

            else
                []
    in
    Html.li wrapperAttributes
        [ Html.a
            [ Attributes.class baseTabCssClasses
            , Attributes.class tabStateClass
            , Events.onClick <| ChangedTab tab
            ]
            [ tab
                |> tabToString
                |> Html.text
            ]
        ]


tabToString : Tab -> String
tabToString tab =
    case tab of
        ContactDetails ->
            "Contact details"

        Settings ->
            "Settings"

        Tags ->
            "Tags"
