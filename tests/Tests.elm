module Tests exposing (..)

import Test exposing (Test, describe, test)
import Types exposing (Point)
import Expect exposing (Expectation)
import Parse

suite : Test
suite =
    describe "Parse"
        [ describe "Parse.parse"
            [ test "comma separated numbers" <|
                \_ ->
                    "1, 22, 333"
                        |> Parse.parse
                        |> expectOk [Point 0 1, Point 1 22, Point 2 333]
            ]
        ]

expectOk : a -> Result b a -> Expectation
expectOk expectedPayload actual =
    Expect.equal (Ok expectedPayload) actual
