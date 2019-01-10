module CSVParser exposing (parse)

{-| Adapted from [Brian Hicks' example](https://gist.github.com/BrianHicks/165554b033eb797e3ed851964ecb3a38)
-}

import Parser exposing ((|.), (|=), Parser)


parse : String -> List (List String)
parse input =
    case parseWithSeparators defaultSeparators input of
        Ok (CSV Plain items) ->
            items
                |> List.filter (not << List.isEmpty)
                |> List.map (List.map String.trim)

        _ ->
            []



-----------------------------------------------------------------------  Private


type alias Row =
    List String


type Plain
    = Plain


type WithNamedFields
    = WithNamedFields Row
    | EmptyHeaders


type CSV a
    = CSV a (List Row)


type alias Separators =
    { value : Char }


defaultSeparators : Separators
defaultSeparators =
    { value = ',' }


parseWithSeparators : Separators -> String -> Result (List Parser.DeadEnd) (CSV Plain)
parseWithSeparators separators raw =
    Parser.run (rows separators) raw


rows : Separators -> Parser (CSV Plain)
rows separators =
    Parser.map (CSV Plain) (Parser.loop [] (rowsHelp separators))


rowsHelp : Separators -> List Row -> Parser (Parser.Step (List Row) (List Row))
rowsHelp separators revRows =
    Parser.oneOf
        [ Parser.end
            |> Parser.map (\_ -> Parser.Done (List.reverse revRows))
        , row separators
            |> Parser.map (\newRow -> Parser.Loop (newRow :: revRows))
        ]


row : Separators -> Parser Row
row separators =
    Parser.loop [] (rowHelp separators)


rowHelp : Separators -> Row -> Parser (Parser.Step Row Row)
rowHelp separators revVals =
    let
        doneWhen : Parser a -> Parser (Parser.Step Row Row)
        doneWhen =
            Parser.map (\_ -> Parser.Done (List.reverse revVals))

        nextWhen : Parser String -> Parser (Parser.Step Row Row)
        nextWhen =
            Parser.map (\newVal -> Parser.Loop (newVal :: revVals))
    in
    Parser.oneOf
        [ doneWhen Parser.end
        , doneWhen (Parser.token "\n")
        , Parser.token (String.fromChar separators.value) |> skipTo revVals
        , nextWhen quotedValue

        -- TODO: token for \r\n after updating elm-format. It automatically
        -- formats to the wrong/old syntax for specifying codepoints in the
        -- version I have installed ATM
        , Parser.chompWhile (\c -> c /= '\n' && c /= separators.value)
            |> Parser.getChompedString
            |> nextWhen
        ]


quotedValue : Parser String
quotedValue =
    Parser.succeed identity
        |. Parser.token "\""
        |= Parser.loop "" quotedValueHelp
        |> Parser.andThen
            (\final ->
                case final of
                    Ok good ->
                        Parser.succeed good

                    Err err ->
                        Parser.problem err
            )


quotedValueHelp : String -> Parser (Parser.Step String (Result String String))
quotedValueHelp soFar =
    let
        subAndLoop : String -> Parser a -> Parser (Parser.Step String b)
        subAndLoop alt parser =
            parser
                |> Parser.map (\_ -> Parser.Loop (soFar ++ alt))
    in
    Parser.oneOf
        [ Parser.end |> Parser.map (\_ -> Parser.Done (Err "I reached the end of the input while trying to parse a quoted string."))
        , Parser.token "\"\"" |> subAndLoop "\""
        , Parser.token "\\\"" |> subAndLoop "\""
        , Parser.token "\\" |> skipTo soFar
        , Parser.token "\""
            |> Parser.map (\_ -> Parser.Done (Ok soFar))
        , Parser.chompWhile (\c -> c /= '\\' && c /= '"')
            |> Parser.getChompedString
            |> Parser.map (\newPortion -> Parser.Loop (soFar ++ newPortion))
        ]


skipTo : b -> Parser a -> Parser (Parser.Step b c)
skipTo soFar =
    Parser.map (\_ -> Parser.Loop soFar)


firstRowAreNames : CSV Plain -> CSV WithNamedFields
firstRowAreNames (CSV _ rowsAndHeader) =
    case rowsAndHeader of
        head :: body ->
            CSV (WithNamedFields head) body

        [] ->
            CSV EmptyHeaders rowsAndHeader
