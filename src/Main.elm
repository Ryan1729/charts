module Main exposing (Flags, Model, ModelError, ModelResult, decodeFlags, init, main)

import Browser
import Chart as C
import Chart.Attributes as CA
import Html exposing (div, output, text, textarea)
import Html.Attributes exposing (class, id, placeholder)
import Json.Decode as JD


main =
    Browser.element
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }


type alias Model =
    {}


type alias ModelError =
    String


type alias ModelResult =
    Result ModelError Model


type alias Flags =
    {}


decodeFlags : JD.Decoder Flags
decodeFlags =
    JD.succeed {}


init : JD.Value -> ( ModelResult, Cmd Msg )
init flags =
    -- We think we'll want localstorage stuff in future, so leave a spot
    -- for that.
    ( case JD.decodeValue decodeFlags flags of
        Ok {} ->
            Ok {}

        Err error ->
            "Error decoding flags: "
                ++ JD.errorToString error
                |> Err
    , Cmd.none
    )


type Msg
    = NoOp


update : Msg -> ModelResult -> ( ModelResult, Cmd Msg )
update msg modelResult =
    case modelResult of
        Err _ ->
            ( modelResult, Cmd.none )

        Ok model ->
            case msg of
                NoOp ->
                    ( Ok model, Cmd.none )


type alias Point =
    { x : Float
    , y : Float
    }


type alias Colour =
    String


red : Colour
red =
    "#de4949"


green : Colour
green =
    "#30b06e"


yellow : Colour
yellow =
    "#ffb937"


blue : Colour
blue =
    "#3352e1"


magenta : Colour
magenta =
    "#533354"


cyan : Colour
cyan =
    "#5a7d8b"


white : Colour
white =
    "#eeeeee"


view modelResult =
    case modelResult of
        Err e ->
            Html.text e

        Ok model ->
            div [ class "responsive-two-column-grid full-h" ]
                [ div [ class "input-wrapper full-h" ]
                    [ textarea [ id "input", placeholder "" ] []
                    ]
                , div [ class "output-wrapper full-h" ]
                    [ output [ id "output" ]
                        [ let
                            data =
                                [ Point 1 2
                                , Point 3 4
                                , Point 5 5.5
                                , Point 2 1
                                , Point 4 3
                                , Point 6 5.5
                                , Point 7 2
                                , Point 8 4
                                , Point 9 5.5
                                , Point 10 12
                                , Point 11 14
                                , Point 12 15.5
                                ]
                          in
                          C.chart
                            [ CA.height 300
                            , CA.width 300
                            ]
                            [ C.grid
                                [ CA.color white
                                ]
                            , C.yTicks [ CA.color white ]
                            , C.xLabels [ CA.color cyan ]
                            , C.yLabels [ CA.color cyan ]
                            , C.series .x
                                [ C.interpolated .y [ CA.color green ] [ CA.circle ]
                                ]
                                data
                            ]
                        ]
                    ]
                ]


subscriptions _ =
    Sub.batch
        []
