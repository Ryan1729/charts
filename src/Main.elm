module Main exposing (Flags, Model, ModelError, ModelResult, decodeFlags, init, main)

import Browser
import Html
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


view modelResult =
    case modelResult of
        Err e ->
            Html.text e

        Ok model ->
            Html.text "TODO"


subscriptions _ =
    Sub.batch
        []
