module Main exposing (main)

import Browser
import Html
import Page.Items as Items


main : Program () Model Msg
main =
    Browser.document
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }


type Model
    = Items Items.Model


type Msg
    = GotItemsMsg Items.Msg


init : () -> ( Model, Cmd Msg )
init _ =
    Items.init |> wrapWith Items GotItemsMsg


wrapWith : (subModel -> Model) -> (subMsg -> Msg) -> ( subModel, Cmd subMsg ) -> ( Model, Cmd Msg )
wrapWith toModel toMsg ( subModel, subCmd ) =
    ( toModel subModel
    , Cmd.map toMsg subCmd
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( msg, model ) of
        ( GotItemsMsg subMsg, Items subModel ) ->
            Items.update subMsg subModel |> wrapWith Items GotItemsMsg


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none


view : Model -> Browser.Document Msg
view model =
    case model of
        Items subModel ->
            { title = "Items!!"
            , body = [ Items.view subModel |> Html.map GotItemsMsg ]
            }
