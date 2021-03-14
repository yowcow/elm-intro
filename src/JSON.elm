module JSON exposing (main)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Json.Decode exposing (Decoder, field, string)


main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }


type Model
    = Init
    | Loading
    | Failure
    | Success String


init : () -> ( Model, Cmd Msg )
init _ =
    --( Loading, getRandomCatGif )
    ( Init, Cmd.none )


type Msg
    = MorePlease
    | GotGif (Result Http.Error String)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        MorePlease ->
            ( Loading, getRandomCatGif )

        GotGif result ->
            case result of
                Ok url ->
                    ( Success url, Cmd.none )

                Err _ ->
                    ( Failure, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none


view : Model -> Html Msg
view model =
    case model of
        Init ->
            div []
                [ text "Hi!  Click to get a cat!"
                , button [ onClick MorePlease ] [ text "Try now!" ]
                ]

        Loading ->
            text "Loading..."

        Failure ->
            div []
                [ text "I could not load a random cat for some reason!"
                , button [ onClick MorePlease ] [ text "Try again!" ]
                ]

        Success url ->
            div []
                [ button [ onClick MorePlease, style "display" "block" ] [ text "More please!" ]
                , img [ src url ] []
                ]


getRandomCatGif : Cmd Msg
getRandomCatGif =
    Http.get
        { url = "https://api.giphy.com/v1/gifs/random?api_key=dc6zaTOxFJmzC&tag=cat"
        , expect = Http.expectJson GotGif gifDecoder
        }


gifDecoder : Decoder String
gifDecoder =
    field "data" (field "image_url" string)
