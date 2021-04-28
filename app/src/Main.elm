module Main exposing (main)

import Browser
import Html exposing (button, div, text)
import Html.Events exposing (onClick)
import Http
import Json.Decode as D


main =
    Browser.document
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }


type alias Model =
    Counter


type Counter
    = Loading
    | Failure
    | Number Int


type Msg
    = Increment
    | Decrement
    | GotResult (Result Http.Error Int)


init : () -> ( Model, Cmd Msg )
init _ =
    ( Loading
    , initNumber ()
    )


initNumber : () -> Cmd Msg
initNumber _ =
    Http.get
        { url = "/init.json"
        , expect = Http.expectJson GotResult (D.field "body" (D.field "count" D.int))
        }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Increment ->
            case model of
                Number 10 ->
                    ( Number 10
                    , initNumber ()
                    )

                Number num ->
                    ( Number (num + 1)
                    , Cmd.none
                    )

                _ ->
                    ( model
                    , Cmd.none
                    )

        Decrement ->
            case model of
                Number 0 ->
                    ( Number 0
                    , initNumber ()
                    )

                Number num ->
                    ( Number (num - 1)
                    , Cmd.none
                    )

                _ ->
                    ( model
                    , Cmd.none
                    )

        GotResult result ->
            case result of
                Ok num ->
                    ( Number num
                    , Cmd.none
                    )

                Err _ ->
                    ( Failure
                    , Cmd.none
                    )


view : Model -> Browser.Document Msg
view model =
    case model of
        Loading ->
            { title = "Loading..."
            , body =
                [ div []
                    [ text "Loading..." ]
                ]
            }

        Failure ->
            { title = "Something went wrong :|"
            , body =
                [ div []
                    [ text "Something went wrong :|"
                    ]
                ]
            }

        Number num ->
            { title = "Count: " ++ String.fromInt num
            , body =
                [ div []
                    [ text ("Count is " ++ String.fromInt num) ]
                , div []
                    [ button [ onClick Decrement ] [ text "Decrement" ]
                    , button [ onClick Increment ] [ text "Increment" ]
                    ]
                ]
            }


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none
