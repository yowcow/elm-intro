module App exposing (main)

import Browser
import Browser.Navigation as Nav
import Html as H
import Html.Attributes as HA
import Html.Lazy as HL
import Url
import Url.Parser as U exposing ((</>), (<?>))
import Url.Parser.Query as Q


main : Program () Model Msg
main =
    Browser.application
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        , onUrlChange = UrlChanged
        , onUrlRequest = LinkClicked
        }


type alias Model =
    { key : Nav.Key
    , url : Url.Url
    , route : Route
    , people : List Person
    }


type Route
    = NotFound
    | Source
    | Home
    | Profiles
    | Profile Int
    | Review String (Maybe Int)


type alias Person =
    { id : Int
    , name : String
    , location : String
    }


init : () -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init _ url key =
    ( Model key url Source initPeople, Cmd.none )


initPeople : List Person
initPeople =
    [ { id = 1, name = "Taro", location = "Tokyo" }
    , { id = 2, name = "Jiro", location = "Antarctica" }
    , { id = 3, name = "Ichiro", location = "Seattle" }
    ]


type Msg
    = LinkClicked Browser.UrlRequest
    | UrlChanged Url.Url


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        LinkClicked urlRequest ->
            case urlRequest of
                Browser.Internal url ->
                    ( model, Nav.pushUrl model.key (Url.toString url) )

                Browser.External href ->
                    ( model, Nav.load href )

        UrlChanged url ->
            ( { model
                | url = url
                , route = toRoute url
              }
            , Cmd.none
            )


toRoute : Url.Url -> Route
toRoute url =
    Maybe.withDefault NotFound (U.parse route url)


route : U.Parser (Route -> a) a
route =
    U.oneOf
        [ U.map Source (U.s "src" </> U.s "App.elm")
        , U.map Home (U.s "home")
        , U.map Profiles (U.s "profiles")
        , U.map Profile (U.s "profiles" </> U.int)
        , U.map Review (U.s "reviews" </> U.string <?> Q.int "page")
        ]


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none


view : Model -> Browser.Document Msg
view model =
    let
        _ =
            Debug.log "model" model
    in
    { title = "URL Interceptor"
    , body =
        [ viewHeader model
        , case model.route of
            Source ->
                HL.lazy viewPage "Source of App.elm"

            Home ->
                HL.lazy viewPage "Home"

            Profiles ->
                HL.lazy viewProfiles model.people

            Profile id ->
                case List.filter (\p -> p.id == id) model.people of
                    [] ->
                        viewNotFound

                    profile :: _ ->
                        -- take the first match
                        HL.lazy viewProfile profile

            Review title page ->
                HL.lazy2 viewReview title page

            _ ->
                viewNotFound
        ]
    }


viewHeader : Model -> H.Html Msg
viewHeader model =
    H.div []
        [ H.text "The current URL is: "
        , H.b [] [ H.text <| Url.toString model.url ]
        , H.h4 [] [ H.text "Links:" ]
        , H.ul []
            [ viewLink "/src/App.elm"
            , viewLink "/home"
            , viewLink "/profiles"
            , viewLink "/reviews/the-centry-of-the-self"
            , viewLink "/reviews/public-opinion?page=123"
            , viewLink "https://github.com/"
            ]
        , H.hr [] []
        ]


viewPage : String -> H.Html Msg
viewPage title =
    H.div []
        [ H.h1 [] [ H.text title ] ]


viewProfiles : List Person -> H.Html Msg
viewProfiles people =
    H.div []
        [ H.h1 [] [ H.text "Profiles" ]
        , H.ul []
            -- prepending a link to non-existing profile
            (viewLink "/profiles/9999"
                :: List.map
                    (\p ->
                        H.li []
                            [ H.a [ HA.href <| "/profiles/" ++ String.fromInt p.id ] [ H.text p.name ] ]
                    )
                    people
            )
        ]


viewProfile : Person -> H.Html Msg
viewProfile p =
    H.div []
        [ H.h1 [] [ H.text <| "Profile: " ++ p.name ]
        , H.dl []
            [ H.dt [] [ H.text "Location:" ]
            , H.dd [] [ H.text p.location ]
            ]
        ]


viewReview : String -> Maybe Int -> H.Html Msg
viewReview title page =
    H.div []
        [ H.h1 [] [ H.text <| "Review: " ++ title ]
        , case page of
            Just p ->
                H.div [] [ H.text <| "Page: " ++ String.fromInt p ]

            Nothing ->
                H.div [] [ H.text "No specific page" ]
        ]


viewNotFound : H.Html Msg
viewNotFound =
    H.div []
        [ H.h1 [] [ H.text "Page Not Found" ] ]


viewLink : String -> H.Html Msg
viewLink path =
    H.li []
        [ H.a [ HA.href path ] [ H.text path ] ]
