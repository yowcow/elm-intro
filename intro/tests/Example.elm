module Example exposing (..)

import Expect
import Test exposing (..)


suite : Test
suite =
    describe "The Example module"
        [ describe "add"
            [ test "add 1 1 = 2" <|
                \_ ->
                    Expect.equal
                        2
                        (add 1 1)
            ]
        , describe "user"
            [ test "user 1 'hoge'" <|
                \_ ->
                    Expect.equal
                        { id = 1, name = "hoge" }
                        (user 1 "hoge")
            ]
        ]


add : Int -> Int -> Int
add x y =
    x + y


type alias User =
    { id : Int
    , name : String
    }


user : Int -> String -> User
user id name =
    User id name
