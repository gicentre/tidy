module CSVParser exposing (parse, parseDelimited)

{-| Adapted from <https://github.com/lovasoa/elm-csv>
-}


parse : String -> List (List String)
parse =
    parseWith ","
        >> mergeWithHeaders


parseDelimited : Char -> String -> List (List String)
parseDelimited delimiter =
    parseWith (String.fromChar delimiter)
        >> mergeWithHeaders



-----------------------------------------------------------------------  Private


type alias Csv =
    { headers : List String
    , records : List (List String)
    }


mergeWithHeaders : Csv -> List (List String)
mergeWithHeaders csv =
    .headers csv :: .records csv


parseWith : String -> String -> Csv
parseWith separator lines =
    let
        values =
            splitWith separator lines

        headers =
            List.head values
                |> Maybe.withDefault []

        records =
            List.drop 1 values
    in
    { headers = headers
    , records = records
    }


split : String -> List (List String)
split =
    splitWith ","


splitWith : String -> String -> List (List String)
splitWith separator lines =
    let
        values =
            String.lines lines
                |> List.filter (\x -> not (String.isEmpty x))
    in
    List.map (splitLineWith separator) values


splitLine : String -> List String
splitLine =
    splitLineWith ","


splitLineWith : String -> String -> List String
splitLineWith separator line =
    parseRemaining separator False line []
        |> List.reverse


parseRemaining : String -> Bool -> String -> List String -> List String
parseRemaining separator quoted remaining done =
    if remaining == "" then
        done

    else if separator /= "" && not quoted && String.startsWith separator remaining then
        let
            newQuoted =
                False

            nextChars =
                String.dropLeft (String.length separator) remaining
        in
        parseRemaining separator False nextChars ("" :: done)

    else
        let
            current =
                List.head done |> Maybe.withDefault ""

            others =
                List.tail done |> Maybe.withDefault []

            nextChar =
                String.slice 0 1 remaining

            nextNextChar =
                String.slice 1 2 remaining

            startQuote =
                nextChar == "\"" && nextNextChar /= "\"" && current == ""

            doubleQuote =
                nextChar == "\"" && nextNextChar == "\""

            isEscapedQuote =
                not quoted && (nextChar == "\\" || nextChar == "\"") && nextNextChar == "\""

            endQuote =
                quoted && nextChar == "\"" && not isEscapedQuote

            newQuoted =
                (quoted && not endQuote) || startQuote

            nextChars =
                String.dropLeft
                    (if isEscapedQuote || doubleQuote then
                        2

                     else
                        1
                    )
                    remaining

            newChar =
                if doubleQuote then
                    ""

                else if isEscapedQuote then
                    "\""

                else if startQuote || endQuote then
                    ""

                else
                    nextChar

            newDone =
                (current ++ newChar) :: others
        in
        parseRemaining separator newQuoted nextChars newDone
