module TextFields exposing (main)

import Browser
import Html exposing (Attribute, Html, div, input, text)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput)


main =
    Browser.sandbox
        { init = init
        , update = update
        , view = view
        }


type alias Model =
    { content : String
    }


init : Model
init =
    { content = "" }


type Msg
    = Change123 String


update : Msg -> Model -> Model
update msg model =
    case msg of
        Change123 newContent ->
            { model | content = newContent }


view : Model -> Html Msg
view model =
    div
        []
        [ input
            [ placeholder "Text to reverse"
            , value model.content
            , onInput Change123
            ]
            []
        , div
            []
            [ text (String.reverse model.content)
            ]
        ]
