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

        extractOutput : Acc -> Output
        extractOutput =
            .output
                >> List.sortBy .x
                >> Ok
    in
    case acc.kind of
        ErrorOut error ->
            Err error

        ExpectDigit last ->
            appendNumber last acc
                |> extractOutput

        ExpectY xNumber yNumber ->
            appendPoint xNumber yNumber acc
                |> extractOutput

        _ ->
            acc
                |> extractOutput


type Kind
    = Initial
    | ExpectDigit Int
    | ExpectX Int
    | ExpectY Int Int
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


setKind : Kind -> Acc -> Acc
setKind kind acc =
    { acc | kind = kind }


parseStep : Char -> Acc -> Acc
parseStep char acc =
    case acc.kind of
        Initial ->
            if Char.isDigit char then
                acc
                    |> setKind (charToIntOr0 char |> ExpectDigit)

            else
                case char of
                    '(' ->
                        acc
                            |> setKind (ExpectX 0)

                    _ ->
                        acc

        ExpectX number ->
            if Char.isDigit char then
                acc
                    |> setKind (ExpectX (number * 10 + charToIntOr0 char))

            else
                case char of
                    _ ->
                        acc
                            |> setKind (ExpectY number 0)

        ExpectY xNumber yNumber ->
            if Char.isDigit char then
                acc
                    |> setKind (ExpectY xNumber (yNumber * 10 + charToIntOr0 char))

            else
                case char of
                    ')' ->
                        appendPoint xNumber yNumber acc

                    _ ->
                        acc

        ExpectDigit number ->
            if Char.isDigit char then
                acc
                    |> setKind (ExpectDigit (number * 10 + charToIntOr0 char))

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


appendPoint xNumber yNumber acc =
    { acc
        | output = Point (toFloat xNumber) (toFloat yNumber) :: acc.output
        , kind = Initial
    }


charToIntOr0 : Char -> Int
charToIntOr0 =
    charToInt >> Maybe.withDefault 0


charToInt : Char -> Maybe Int
charToInt =
    String.fromChar
        >> String.toInt
