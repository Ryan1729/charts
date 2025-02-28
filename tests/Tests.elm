module Tests exposing (..)

import Test exposing (Test, describe, test)
import Types exposing (Point)
import Expect exposing (Expectation)
import Parse

suite : Test
suite =
    describe "Parse"
        [ describe "Parse.parse"
            [ test "bare numbers" <|
                \_ ->
                    "1 22 333"
                        |> Parse.parse
                        |> expectOk [Point 0 1, Point 1 22, Point 2 333]
            , test "comma separated numbers" <|
                \_ ->
                    "1, 22, 333"
                        |> Parse.parse
                        |> expectOk [Point 0 1, Point 1 22, Point 2 333]
            , test "comma separated numbers with brackets" <|
                \_ ->
                    "[1, 22, 333]"
                        |> Parse.parse
                        |> expectOk [Point 0 1, Point 1 22, Point 2 333]
            , test "comma separated ordered tuples" <|
                \_ ->
                    "(0, 1), (1, 22), (2, 333)"
                        |> Parse.parse
                        |> expectOk [Point 0 1, Point 1 22, Point 2 333]
            , test "comma separated unordered tuples" <|
                \_ ->
                    "(2, 1), (1, 22), (0, 333)"
                        |> Parse.parse
                        |> expectOk [Point 0 333, Point 1 22, Point 2 1]
            , test "sparse comma separated unordered tuples" <|
                \_ ->
                    "(4, 1), (1, 22), (8, 333)"
                        |> Parse.parse
                        |> expectOk [Point 1 22, Point 4 1, Point 8 333]
            ]
        ]

expectOk : a -> Result b a -> Expectation
expectOk expectedPayload actual =
    Expect.equal (Ok expectedPayload) actual
