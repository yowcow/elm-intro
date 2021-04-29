module Main exposing (main)

import Browser
import Html exposing (Html, button, dd, div, dl, dt, input, li, text, ul)
import Html.Attributes exposing (checked, style, type_)
import Html.Events exposing (onCheck, onClick)
import Http
import Json.Decode as D


main : Program () Model Msg
main =
    Browser.document
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }


type Model
    = Loading
    | Failure
    | Page { count : Int, items : MyItems }


type alias MyItem =
    { id : Int
    , title : String
    , done : Bool
    }


type alias MyItems =
    List MyItem


type alias MyResponse =
    { success : Bool
    , count : Int
    , items : MyItems
    }


type Msg
    = Load
    | GotResult (Result Http.Error MyResponse)
    | ToggleDone Int Bool


init : () -> ( Model, Cmd Msg )
init _ =
    ( Loading
    , loadPage ()
    )


loadPage : () -> Cmd Msg
loadPage _ =
    Http.get
        { url = "/init.json"
        , expect = Http.expectJson GotResult decodeResponse
        }


decodeResponse : D.Decoder MyResponse
decodeResponse =
    D.map3 MyResponse
        (D.field "status"
            (D.field "success" D.bool)
        )
        (D.field "body"
            (D.field "count" D.int)
        )
        (D.field "body"
            (D.field "items" itemsDecoder)
        )


itemsDecoder : D.Decoder MyItems
itemsDecoder =
    D.list itemDecoder


itemDecoder : D.Decoder MyItem
itemDecoder =
    D.map3 MyItem
        (D.field "id" D.int)
        (D.field "title" D.string)
        (D.field "done" D.bool)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Load ->
            ( model
            , loadPage ()
            )

        GotResult result ->
            case result of
                Ok input ->
                    if input.success then
                        ( Page { count = input.count, items = input.items }
                        , Cmd.none
                        )

                    else
                        ( Failure
                        , Cmd.none
                        )

                Err _ ->
                    ( Failure
                    , Cmd.none
                    )

        ToggleDone id checked ->
            case model of
                Page data ->
                    ( Page { data | items = List.map (toggleDone id checked) data.items }
                    , Cmd.none
                    )

                _ ->
                    ( model
                    , Cmd.none
                    )


toggleDone : Int -> Bool -> MyItem -> MyItem
toggleDone id checked item =
    if id == item.id then
        { item | done = checked }

    else
        item


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

        Page data ->
            { title = "Total Items: " ++ String.fromInt data.count
            , body =
                [ div []
                    [ text ("Item count is " ++ String.fromInt data.count) ]
                , ul []
                    (List.map viewItem data.items)
                , div []
                    [ button [ onClick Load ] [ text "Reload" ] ]
                ]
            }


viewItem : MyItem -> Html Msg
viewItem item =
    li
        (if item.done then
            [ style "background-color" "#ddd" ]

         else
            []
        )
        [ dl []
            [ dt [] [ text "ID" ]
            , dd [] [ String.fromInt item.id |> text ]
            , dt [] [ text "Title" ]
            , dd [] [ text item.title ]
            , dt [] [ text "Done?" ]
            , dd []
                [ input
                    [ type_ "checkbox"
                    , checked item.done
                    , onCheck (ToggleDone item.id)
                    ]
                    []
                ]
            ]
        ]


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none
