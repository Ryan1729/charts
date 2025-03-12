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


type alias FloatAcc =
    { whole : String
    , frac : String -- Gotta be string to account for zero in 1.01
    , isNegative : Bool
    , sawPoint : Bool
    }


floatAccInit : FloatAcc
floatAccInit =
    { whole = ""
    , frac = ""
    , isNegative = False
    , sawPoint = False
    }


accumChar : Char -> FloatAcc -> FloatAcc
accumChar char { whole, frac, isNegative, sawPoint } =
    if not isNegative && not sawPoint && char == '-' then
        { whole = whole
        , sawPoint = sawPoint
        , isNegative = True
        , frac = frac
        }

    else if char == '.' then
        { whole = whole
        , sawPoint = True
        , isNegative = isNegative
        , frac = frac
        }

    else if sawPoint then
        { whole = whole
        , sawPoint = sawPoint
        , frac = frac ++ String.fromChar char
        , isNegative = isNegative
        }

    else
        { whole = whole ++ String.fromChar char
        , sawPoint = sawPoint
        , frac = frac
        , isNegative = isNegative
        }


isFloatChar : Char -> Bool
isFloatChar char =
    Char.isDigit char || char == '.' || char == '-'


extractFloat : FloatAcc -> Float
extractFloat { isNegative, whole, frac } =
    let
        w =
            if whole == "" then
                "0"

            else
                whole

        f =
            if frac == "" then
                "0"

            else
                frac
    in
    String.toFloat
        ((if isNegative then
            "-"

          else
            ""
         )
            ++ w
            ++ "."
            ++ f
        )
        |> Maybe.withDefault 0.0


type Kind
    = Initial
    | ExpectDigit FloatAcc
    | ExpectX FloatAcc
    | ExpectY FloatAcc FloatAcc
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
            if isFloatChar char then
                acc
                    |> setKind (accumChar char floatAccInit |> ExpectDigit)

            else
                case char of
                    '(' ->
                        acc
                            |> setKind (ExpectX floatAccInit)

                    _ ->
                        acc

        ExpectX x ->
            if isFloatChar char then
                acc
                    |> setKind (ExpectX (accumChar char x))

            else
                case char of
                    _ ->
                        acc
                            |> setKind (ExpectY x floatAccInit)

        ExpectY x y ->
            if isFloatChar char then
                acc
                    |> setKind (ExpectY x (accumChar char y))

            else
                case char of
                    ')' ->
                        appendPoint x y acc

                    _ ->
                        acc

        ExpectDigit number ->
            if isFloatChar char then
                acc
                    |> setKind (ExpectDigit (accumChar char number))

            else
                case char of
                    _ ->
                        appendNumber number acc

        ErrorOut _ ->
            acc


appendNumber : FloatAcc -> Acc -> Acc
appendNumber number acc =
    { acc
      -- TODO Calling List.length here is kinda dumb perfwise. Storing
      -- the length as an int would be a Big-O improvement
        | output = Point (List.length acc.output |> toFloat) (extractFloat number) :: acc.output
        , kind = Initial
    }


appendPoint xNumber yNumber acc =
    { acc
        | output = Point (extractFloat xNumber) (extractFloat yNumber) :: acc.output
        , kind = Initial
    }


charToIntOr0 : Char -> Int
charToIntOr0 =
    charToInt >> Maybe.withDefault 0


charToInt : Char -> Maybe Int
charToInt =
    String.fromChar
        >> String.toInt
