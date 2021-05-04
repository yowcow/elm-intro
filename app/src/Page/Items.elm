module Page.Items exposing (Model, Msg, init, update, view)

import Html as H
import Html.Attributes as A
import Html.Events as E
import Html.Keyed as Keyed
import Html.Lazy as Lazy
import Http
import Json.Decode as D


type Model
    = Loading
    | Failure
    | Items Int (List Item)


type alias Item =
    { id : Int
    , title : String
    , done : Bool
    }


type Msg
    = LoadItems
    | GotItems (Result Http.Error ItemsResponse)
    | ToggleDone Int Bool


type alias ItemsResponse =
    { success : Bool
    , count : Int
    , items : List Item
    }


init : ( Model, Cmd Msg )
init =
    ( Loading, fetchItemsRequest )


fetchItemsRequest : Cmd Msg
fetchItemsRequest =
    Http.get
        { url = "/init.json"
        , expect = Http.expectJson GotItems itemsResponseDecoder
        }


itemsResponseDecoder : D.Decoder ItemsResponse
itemsResponseDecoder =
    D.map3 ItemsResponse
        (D.field "status"
            (D.field "success" D.bool)
        )
        (D.field "body"
            (D.field "count" D.int)
        )
        (D.field "body"
            (D.field "items" itemsDecoder)
        )


itemsDecoder : D.Decoder (List Item)
itemsDecoder =
    D.list itemDecoder


itemDecoder : D.Decoder Item
itemDecoder =
    D.map3 Item
        (D.field "id" D.int)
        (D.field "title" D.string)
        (D.field "done" D.bool)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        _ =
            Debug.log "msg" msg
    in
    case msg of
        LoadItems ->
            ( Loading, fetchItemsRequest )

        GotItems result ->
            case result of
                Ok input ->
                    if input.success then
                        ( Items input.count input.items
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
                Items count items ->
                    ( Items count (List.map (toggleDone id checked) items)
                    , Cmd.none
                    )

                _ ->
                    ( model, Cmd.none )


toggleDone : Int -> Bool -> Item -> Item
toggleDone id checked item =
    if id == item.id then
        { item | done = checked }

    else
        item


view : Model -> H.Html Msg
view model =
    case model of
        Loading ->
            H.h1 [] [ H.text "Loading..." ]

        Failure ->
            H.h1 [] [ H.text "Failure!!" ]

        Items count items ->
            H.div []
                [ H.h1 [] [ H.text <| "Got " ++ String.fromInt count ++ " Items!!" ]
                , viewItems items
                , H.button [ E.onClick LoadItems ]
                    [ H.text "Click me to reload!!" ]
                ]


viewItems : List Item -> H.Html Msg
viewItems items =
    Keyed.node "ul"
        []
        (List.map viewKeyedItem items)


viewKeyedItem : Item -> ( String, H.Html Msg )
viewKeyedItem item =
    ( String.fromInt item.id, Lazy.lazy viewItem item )


viewItem : Item -> H.Html Msg
viewItem item =
    H.li
        (if item.done then
            [ A.style "background-color" "#ddd" ]

         else
            []
        )
        [ H.dl []
            [ H.dt [] [ H.text "ID" ]
            , H.dd [] [ H.text <| String.fromInt item.id ]
            , H.dt [] [ H.text "Title" ]
            , H.dd [] [ H.text item.title ]
            , H.dt [] [ H.text "Done?" ]
            , H.dd []
                [ H.input
                    [ A.type_ "checkbox"
                    , A.checked item.done
                    , E.onCheck <| ToggleDone item.id
                    ]
                    []
                ]
            ]
        ]
