module ClickToEdit exposing (main)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onBlur, onClick, onInput)


main : Program () Model Msg
main =
    Browser.sandbox
        { init = init
        , view = view
        , update = update
        }


type alias User =
    { name : String
    , email : String
    }


type alias FormState =
    { name : Bool
    , email : Bool
    }


type alias Model =
    { user : User
    , formstate : FormState
    }


type Msg
    = EnableNameInput
    | DisableNameInput
    | ChangeName String
    | EnableEmailInput
    | DisableEmailInput
    | ChangeEmail String


init : Model
init =
    { user = User "hoge" "fuga"
    , formstate = FormState False False
    }


view : Model -> Html Msg
view model =
    dl []
        [ dt [] [ text "Name" ]
        , dd []
            [ viewInput "text"
                "Name"
                model.user.name
                model.formstate.name
                EnableNameInput
                DisableNameInput
                ChangeName
            ]
        , dt [] [ text "Email" ]
        , dd []
            [ viewInput
                "text"
                "Email"
                model.user.email
                model.formstate.email
                EnableEmailInput
                DisableEmailInput
                ChangeEmail
            ]
        ]


viewInput : String -> String -> String -> Bool -> Msg -> Msg -> (String -> Msg) -> Html Msg
viewInput inputType inputPlaceholder inputValue inputState toEditMsg toUneditMsg toChangeMsg =
    if inputState then
        input
            [ type_ inputType
            , placeholder inputPlaceholder
            , value inputValue
            , onInput toChangeMsg
            , onBlur toUneditMsg
            ]
            []

    else
        div [ onClick toEditMsg ] [ text inputValue ]


update : Msg -> Model -> Model
update msg model =
    case msg of
        EnableNameInput ->
            { model
                | formstate =
                    updateFormStateName True model.formstate
            }

        DisableNameInput ->
            { model
                | formstate =
                    updateFormStateName False model.formstate
            }

        ChangeName name ->
            { model
                | user =
                    updateUserName name model.user
            }

        EnableEmailInput ->
            { model
                | formstate =
                    updateFormStateEmail True model.formstate
            }

        DisableEmailInput ->
            { model
                | formstate =
                    updateFormStateEmail False model.formstate
            }

        ChangeEmail email ->
            { model
                | user =
                    updateUserEmail email model.user
            }


updateFormStateName : Bool -> FormState -> FormState
updateFormStateName state formstate =
    { formstate | name = state }


updateFormStateEmail : Bool -> FormState -> FormState
updateFormStateEmail state formstate =
    { formstate | email = state }


updateUserName : String -> User -> User
updateUserName name user =
    { user | name = name }


updateUserEmail : String -> User -> User
updateUserEmail email user =
    { user | email = email }
