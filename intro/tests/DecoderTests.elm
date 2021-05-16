module DecoderTests exposing (..)

import Expect
import Json.Decode as D
import Test exposing (..)


currentObservationDecoderTest : Test
currentObservationDecoderTest =
    test "decodes currentObservation" <|
        \_ ->
            let
                inputJson =
                    """
                        { "city": "Boston",
                          "state": "MA",
                          "temp_f": 98.6 }
                    """

                output =
                    D.decodeString currentObservationDecoder inputJson
            in
            Expect.equal output
                (Ok { city = "Boston", state = "MA", tempF = 98.6 })


weatherConditionsDecoderTest : Test
weatherConditionsDecoderTest =
    test "decode weatherConditions" <|
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
                    Ok
                        { currentObservation =
                            { city = "Tokyo", state = "Tokyo", tempF = 68.456 }
                        }
            in
            Expect.equal (inputJson |> D.decodeString weatherConditionsDecoder) expected


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
