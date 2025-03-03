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
            , test "basic float example" <|
                \_ ->
                    "0.0, 0.1, 2.2"
                        |> Parse.parse
                        |> expectOk [Point 0 0, Point 1 0.1, Point 2 2.2]
            , test "basic float tuple example" <|
                \_ ->
                    "(0, 0.0), (1, 0.1), (2, 2.2)"
                        |> Parse.parse
                        |> expectOk [Point 0 0, Point 1 0.1, Point 2 2.2]
            , test "multi-digit float example" <|
                \_ ->
                    "25.25, 125.125"
                        |> Parse.parse
                        |> expectOk [Point 0 25.25, Point 1 125.125]
            , test "found float example" <|
                \_ ->
                    "[(0,100),(1,87),(2,74),(3,61),(4,48.296295),(5,42.77778),(6,43.48148),(7,47.666668),(8,54.074074),(9,61.88889),(10,70.74074),(11,80.40741),(12,90.666664),(13,101.296295),(14,112.22222),(15,123.51852),]"
                        |> Parse.parse
                        |> expectOk [ Point 0 100, Point 1 87, Point 2 74, Point 3 61, Point 4 48.296295, Point 5 42.77778, Point 6 43.48148, Point 7 47.666668, Point 8 54.074074, Point 9 61.88889, Point 10 70.74074, Point 11 80.40741, Point 12 90.666664, Point 13 101.296295, Point 14 112.22222, Point 15 123.51852]
            
            ]
        ]

expectOk : a -> Result b a -> Expectation
expectOk expectedPayload actual =
    Expect.equal (Ok expectedPayload) actual
