module Main exposing (Flags, Model, ModelError, ModelResult, decodeFlags, init, main)

import Browser
import Chart as C
import Chart.Attributes as CA
import Html exposing (div, output, text, textarea)
import Html.Attributes exposing (class, id, placeholder, value)
import Html.Events exposing (onInput)
import Json.Decode as JD
import Parse exposing (parse)
import Types exposing (Point)


main =
    Browser.element
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }


type alias Model =
    { input : String
    }


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
            Ok { input = "" }

        Err error ->
            "Error decoding flags: "
                ++ JD.errorToString error
                |> Err
    , Cmd.none
    )


type Msg
    = NoOp
    | Input String


update : Msg -> ModelResult -> ( ModelResult, Cmd Msg )
update msg modelResult =
    case modelResult of
        Err _ ->
            ( modelResult, Cmd.none )

        Ok model ->
            case msg of
                NoOp ->
                    ( Ok model, Cmd.none )

                Input input ->
                    ( Ok { model | input = input }, Cmd.none )


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

        Ok { input } ->
            let
                result =
                    parse input
            in
            div [ class "responsive-two-column-grid full-h" ]
                [ div [ class "input-wrapper full-h" ]
                    [ textarea [ id "input", placeholder "", value input, onInput Input ] []
                    ]
                , div [ class "output-wrapper full-h" ]
                    [ output [ id "output" ]
                        [ case result of
                            Err error ->
                                text error

                            Ok data ->
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
