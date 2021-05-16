module DecoderTests exposing (..)

import Expect
import Json.Decode as D
import Test exposing (..)


success : Result a b -> Bool
success result =
    case result of
        Ok _ ->
            True

        Err _ ->
            False


currentObservationDecoderTest : Test
currentObservationDecoderTest =
    describe "currentObservationDecoder" <|
        [ test "OK" <|
            \_ ->
                let
                    inputJson =
                        """
                            { "city": "Boston",
                              "state": "MA",
                              "temp_f": 98.6 }
                        """

                    expected =
                        True
                in
                Expect.equal (D.decodeString currentObservationDecoder inputJson |> success) expected
        , test "missing key is not OK" <|
            \_ ->
                let
                    inputJson =
                        "{}"

                    expected =
                        False
                in
                Expect.equal (D.decodeString currentObservationDecoder inputJson |> success) expected
        , test "wrong temp_f types is not OK" <|
            \_ ->
                let
                    inputJson =
                        """
                            {"city": "Boston",
                             "state": "MA",
                             "temp_f": "98.6"
                            }
                        """

                    expected =
                        False
                in
                Expect.equal (D.decodeString currentObservationDecoder inputJson |> success) expected
        ]


weatherConditionsDecoderTest : Test
weatherConditionsDecoderTest =
    describe "weatherConditionsDecoder" <|
        [ test "OK" <|
            \_ ->
                let
                    inputJson =
                        """
                            {
                                "current_observations": {
                                    "city": "Tokyo",
                                    "state": "Tokyo",
                                    "temp_f": 68.456
                                }
                            }
                        """

                    expected =
                        True
                in
                Expect.equal (inputJson |> D.decodeString weatherConditionsDecoder |> success) expected
        , test "empty JSON is Not OK" <|
            \_ ->
                let
                    inputJson =
                        ""

                    expected =
                        False
                in
                Expect.equal (inputJson |> D.decodeString weatherConditionsDecoder |> success) expected
        , test "no current_observations is not OK" <|
            \_ ->
                let
                    inputJson =
                        "{}"

                    expected =
                        False
                in
                Expect.equal (inputJson |> D.decodeString weatherConditionsDecoder |> success) expected
        ]


type alias WeatherConditions =
    { currentObservation : CurrentObservation }


type alias CurrentObservation =
    { city : String
    , state : String
    , tempF : Float
    }


weatherConditionsDecoder : D.Decoder WeatherConditions
weatherConditionsDecoder =
    D.map WeatherConditions
        (D.field "current_observations" currentObservationDecoder)


currentObservationDecoder : D.Decoder CurrentObservation
currentObservationDecoder =
    D.map3 CurrentObservation
        (D.field "city" D.string)
        (D.field "state" D.string)
        (D.field "temp_f" D.float)
