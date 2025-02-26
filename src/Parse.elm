module Parse exposing (Output, parse)

import Types exposing (Point)


type alias Output =
    Result Error Parsed


type alias Error =
    String


type alias Parsed =
    List Point


parse : String -> Output
parse input =
    let
        acc =
            String.foldl parseStep accInit input
    in
    case acc.kind of
        ErrorOut error ->
            Err error

        ExpectDigit last ->
            appendNumber last acc
                |> .output
                |> List.reverse
                |> Ok

        _ ->
            acc.output
                |> List.reverse
                |> Ok


type Kind
    = Initial
    | ExpectDigit Int
    | ErrorOut Error


type alias Acc =
    { output : Parsed
    , kind : Kind
    }


accInit : Acc
accInit =
    { output = []
    , kind = Initial
    }


setKind : Acc -> Kind -> Acc
setKind acc kind =
    { acc | kind = kind }


parseStep : Char -> Acc -> Acc
parseStep char acc =
    case acc.kind of
        Initial ->
            if Char.isDigit char then
                charToIntOr0 char
                    |> ExpectDigit
                    |> setKind acc

            else
                case char of
                    _ ->
                        acc

        ExpectDigit number ->
            if Char.isDigit char then
                ExpectDigit (number * 10 + charToIntOr0 char)
                    |> setKind acc

            else
                case char of
                    _ ->
                        appendNumber number acc

        ErrorOut _ ->
            acc


appendNumber number acc =
    { acc
      -- TODO Calling List.length here is kinda dumb perfwise. Storing
      -- the length as an int would be a Big-O improvement
        | output = Point (List.length acc.output |> toFloat) (toFloat number) :: acc.output
        , kind = Initial
    }


charToIntOr0 : Char -> Int
charToIntOr0 =
    charToInt >> Maybe.withDefault 0


charToInt : Char -> Maybe Int
charToInt =
    String.fromChar
        >> String.toInt
