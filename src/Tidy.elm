module Tidy exposing
    ( Table
    , Heading
    , Cell
    , fromCSV
    , fromGrid
    , tableSummary
    , melt
    , leftJoin
    , rightJoin
    , innerJoin
    , outerJoin
    , filterRows
    , filterColumns
    , numColumn
    , strColumn
    , boolColumn
    , toColumn
    )

{-| A collection of utilities for representing tabular data and reshaping them.


## Table Representation

@docs Table
@docs Heading
@docs Cell
@docs fromCSV
@docs fromGrid
@docs tableSummary


## Table Tidying

[Tidy data](https://www.jstatsoft.org/index.php/jss/article/view/v059i10/v59i10.pdf)
is a convention for organising tabular data such that columns represent _variables_
and rows represent _observations_. This greatly simplifies data interchange and
many data analytical functions.

Common problems with data that are not in tidy format ("messy" data) include:

  - Column headers are values not variable names
  - Multiple variables are stored in the same column
  - Variables arranged in rows as well as columns
  - Multiple types of observational unit are stored in the same table
  - The same observational unit is stored in multiple tables.

Messy data can be tidied with a small number of simple operations.

@docs melt


## Table Joining

Join two tables using a common key. While not specific to tidy data, joining tidy
tables is often more meaningful than joining messy ones. The examples below illustrate
joining two input tables as follows with shared key values k2 and k4:

```markdown
table1:

| Key | colLabelA | colLabelB |
| --- | --------- | --------- |
| k1  | a1        | b1        |
| k2  | a2        | b2        |
| k3  | a3        | b3        |
| k4  | a4        | b4        |

table2:

| Key | colLabelC | colLabelD |
| --- | --------- | --------- |
| k2  | c2        | d2        |
| k4  | c4        | d4        |
| k6  | c6        | d6        |
| k8  | c8        | d8        |
```

@docs leftJoin
@docs rightJoin
@docs innerJoin
@docs outerJoin


## Table Filtering

@docs filterRows
@docs filterColumns


## Column conversion

@docs numColumn
@docs strColumn
@docs boolColumn
@docs toColumn

-}

import Dict exposing (Dict)
import Regex


{-| A table of data arranged in rows and columns.
-}
type
    Table
    -- Opaque type
    = Table
        { columns : Columns
        }


{-| The columns that make up a table of cells.
-}
type alias Columns =
    Dict Heading (List Cell)


{-| Type used to represent table column headings.
-}
type alias Heading =
    String


{-| Type of data stored in the cells that make up a table.
-}
type alias Cell =
    String


{-| Create a table from a multi-line comma-separated string in the form:

    """colLabelA,colLabelB,colLabelC,etc.
       a1,b1,c1, etc.
       a2,b2,c2, etc.
       a3,b3,c3, etc.
       etc."""

-}
fromCSV : String -> Table
fromCSV =
    let
        submatches : String -> String -> List String
        submatches regex =
            Regex.find
                (Regex.fromString regex |> Maybe.withDefault Regex.never)
                >> List.concatMap .submatches
                >> List.filterMap identity

        addEntry : List Cell -> Columns -> Columns
        addEntry xs =
            case xs of
                hd :: tl ->
                    Dict.insert (String.trim hd) (List.map String.trim tl)

                _ ->
                    identity
    in
    -- regex modified from https://stackoverflow.com/a/42535295 to account for
    -- whitespace between commas adjacent to quoted entries.
    -- For Elm parser approach consider
    -- https://gist.github.com/BrianHicks/165554b033eb797e3ed851964ecb3a38
    String.lines
        >> List.filter (not << String.isEmpty)
        >> List.map (\s -> " " ++ s ++ " ")
        >> List.map (submatches "(?:,\\s*\"|^\")(\"\"|[\\w\\W]*?)(?=\"\\s*,|\"$)|(?:,(?!\")|^(?!\"))([^,]*?)(?=$|,)")
        >> transpose
        >> List.foldl addEntry Dict.empty
        >> toTable


{-| Transform row-prime grid of input values in the form:

    """
       z00,z01,z02,z03, etc.
       z10,z11,z12,z13, etc.
       z20,z21,z22,c23, etc.
       z30,z31,z32,c33, etc.
       etc."""

into a tidy table in the form:

```markdown
| row | col |   z |
| --- | --- | --- |
|   0 |   0 | z00 |
|   0 |   1 | z00 |
|   0 |   2 | z00 |
|   0 |   3 | z00 |
|   1 |   0 | z10 |
|   1 |   1 | z11 |
|   : |   : |   : |
```

-}
fromGrid : String -> Table
fromGrid =
    let
        updateCol v maybeCol =
            case maybeCol of
                Nothing ->
                    Just [ v ]

                Just col ->
                    Just (v :: col)

        addRow ( r, c, z ) =
            Dict.update "row" (updateCol r)
                >> Dict.update "col" (updateCol c)
                >> Dict.update "z" (updateCol z)
    in
    String.lines
        >> List.filter (not << String.isEmpty)
        >> List.indexedMap
            (\r ->
                String.split ","
                    >> List.filter (not << String.isEmpty)
                    >> List.indexedMap
                        (\c z ->
                            ( String.fromInt r, String.fromInt c, String.trim z )
                        )
            )
        >> List.concat
        >> List.foldl addRow Dict.empty
        >> toTable


{-| Extract the values of a given column from a table. The type of values in the
column is determined by the given cell conversion function. The converter function
should handle cases of missing data in the table as well as failed conversions
(e.g. attempts to convert text into a number).

    imputeMissing : Cell -> Int
    imputeMissing =
        String.toFloat >> Maybe.withDefault 0

    myTable |> toColumn "count" imputeMissing

-}
toColumn : Heading -> (Cell -> a) -> Table -> List a
toColumn heading converter =
    getColumns
        >> Dict.get heading
        >> Maybe.withDefault []
        >> List.map converter


{-| Extract the numeric values of a given column from a table. Any conversions that
fail, including missing values in the table are converted into zeros. If you wish
to handle missing data / failed conversions in a different way, use
[toColumn](#toColumn) instead, providing a custom converter function.

    myTable |> numColumn "year"

-}
numColumn : Heading -> Table -> List Float
numColumn heading =
    toColumn heading (String.toFloat >> Maybe.withDefault 0)


{-| Extract the string values of a given column from a table. Missing values in
the table are represented as empty strings. If you wish to handle missing values
in a different way, use [toColumn](#toColumn) instead, providing a custom converter
function.

    myTable |> strColumn "cityName"

-}
strColumn : Heading -> Table -> List String
strColumn heading =
    toColumn heading identity


{-| Extract Boolean values of a given column from a table. Assumes that `True`
values can be represented by the case-insenstive strings `true`, `yes` and `1`
while all other values are assumed to be false.

      myTable |> toBool "isMarried"

-}
boolColumn : Heading -> Table -> List Bool
boolColumn heading =
    let
        toBool str =
            case str |> String.trim |> String.toLower of
                "true" ->
                    True

                "yes" ->
                    True

                "1" ->
                    True

                _ ->
                    False
    in
    toColumn heading toBool


{-| Provide a textual description of a table, configurable to show a given number
of table rows. If the number of rows to show is negative, all rows are output.
This is designed primarily to generate markdown output, but should be interpretable
as raw text.
-}
tableSummary : Int -> Table -> List String
tableSummary maxRows tbl =
    let
        mx =
            if maxRows < 0 then
                numTableRows

            else
                maxRows

        addDividers =
            List.map (\s -> s ++ " |") >> (::) "|"

        headings =
            tbl |> getColumns |> Dict.keys |> addDividers

        divider =
            tbl |> getColumns |> Dict.keys |> List.map (always "-") |> addDividers

        values =
            tbl
                |> getColumns
                |> Dict.values
                |> List.map (List.take mx)
                |> transpose
                |> List.map (\ss -> addDividers ss ++ [ "\n" ])
                |> List.concat

        numTableRows =
            List.foldl (max << List.length) 0 (Dict.values (getColumns tbl))

        continues =
            if numTableRows > mx then
                tbl |> getColumns |> Dict.keys |> List.map (always " : ") |> addDividers

            else
                []

        dimensions =
            [ "\n", String.fromInt numTableRows, " rows and ", String.fromInt (Dict.size (getColumns tbl)), " columns in total." ]
    in
    [ headings, [ "\n" ], divider, [ "\n" ], values, continues, dimensions ]
        |> List.concat


{-| Combine several columns that represent the same variable into two columns, one
referencing the original column, the other the values of the variable. For example
the following messy table

```markdown
| location  | temperature2017 | temperature2018 |
| --------- | --------------- | --------------- |
| Bristol   | 12              | 14              |
| Sheffield | 11              | 13              |
| Glasgow   |  8              |  9              |
```

can be melted to create a tidy table:

```markdown
| location  | year | temperature |
| --------- | ---- | ----------- |
| Bristol   | 2017 | 12          |
| Bristol   | 2018 | 14          |
| Sheffield | 2017 | 11          |
| Sheffield | 2018 | 13          |
| Glasgow   | 2017 |  8          |
| Glasgow   | 2017 |  9          |
```

The first two parameters represent the heading names to be given to the column
reference (`year` in the example above) and variable column (`temperature` in the
example above) to be generated. The third is a list of the (columnName,columnReference)
to be melted (e.g. `[ ("temperature2017", "2017"), ("temperature2018", "2017") ]`
above) and the final, the table to convert. For example

    """location,temperature2017,temperature2017
    Bristol,12,14
    Sheffield,11,13
    Glasgow, 8,9"""
        |> fromCSV
        |> melt "year"
            "temperature"
            [ ( "temperature2017", "2017" )
            , ( "temperature2018", "2018" )
            ]

-}
melt : Heading -> Heading -> List ( Heading, Cell ) -> Table -> Table
melt columnName valueName colVars table =
    let
        colToVarLookup : Dict Heading Cell
        colToVarLookup =
            Dict.fromList colVars

        emptyColumns : Columns
        emptyColumns =
            table
                |> getColumns
                |> Dict.keys
                |> List.filter (\s -> not (List.member s (List.map Tuple.first colVars)))
                |> (++) [ columnName, valueName ]
                |> List.map (\label -> ( label, [] ))
                |> Dict.fromList

        newRows : Columns -> List (List ( Heading, Cell ))
        newRows columns =
            let
                meltCol ( oldHeading, val ) =
                    case Dict.get oldHeading colToVarLookup of
                        Just colRef ->
                            Just [ ( columnName, colRef ), ( valueName, val ) ]

                        Nothing ->
                            Nothing

                unmeltedCol ( oldHeading, val ) =
                    case Dict.get oldHeading colToVarLookup of
                        Just colRef ->
                            Nothing

                        Nothing ->
                            Just ( oldHeading, val )

                unmeltedCols =
                    columns |> columnsHead |> List.filterMap unmeltedCol
            in
            columns
                |> columnsHead
                |> List.filterMap meltCol
                |> List.map (\x -> x ++ unmeltedCols)

        addToColumn : Heading -> Columns -> Cell -> List Cell
        addToColumn heading columns val =
            (Dict.get heading columns |> Maybe.withDefault []) ++ [ val ]

        addMeltedRows : List ( Heading, Cell ) -> Columns -> Columns
        addMeltedRows row columns =
            List.foldl (\( heading, val ) -> Dict.insert heading (addToColumn heading columns val)) columns row

        extractRows : ( Columns, Columns ) -> ( Columns, Columns )
        extractRows ( oldColumns, newColumns ) =
            case columnsHead oldColumns of
                [] ->
                    ( oldColumns, newColumns )

                row ->
                    extractRows ( columnsTail oldColumns, List.foldr addMeltedRows newColumns (newRows oldColumns) )
    in
    extractRows ( getColumns table, emptyColumns ) |> Tuple.second |> toTable


{-| Keep rows in the table where the values in the given column satisfy the given
test. The test should be a function that takes a cell value and returns either
`True` or `False` depending on whether the row containing that value in the column
should be retained.

    isWarm : Cell -> Bool
    isWarm s =
        case String.toFloat s of
            Just x ->
                x >= 10

            Nothing ->
                False

    warmCities =
        myTable |> filterRows "temperature" isWarm

-}
filterRows : Heading -> (Cell -> Bool) -> Table -> Table
filterRows columnName fn tbl =
    let
        predicate n val =
            if fn val then
                n

            else
                -1

        colValues =
            case Dict.get columnName (getColumns tbl) of
                Nothing ->
                    []

                Just values ->
                    List.indexedMap predicate values
                        |> List.filter (\n -> n /= -1)

        filterFromCol key val =
            val
                |> List.indexedMap Tuple.pair
                |> List.filter (\( n, _ ) -> List.member n colValues)
                |> List.map Tuple.second
    in
    tbl |> getColumns |> Dict.map filterFromCol |> toTable


{-| Keep columns in the table whose names satisfy the given test. The test should
be a function that takes a column heading and returns either `True` or `False`
depending on whether the column should be retained.

    myTable |> filterColumns ((==) "temperature2017")

-}
filterColumns : (Heading -> Bool) -> Table -> Table
filterColumns fn =
    getColumns >> Dict.filter (\k _ -> fn k) >> toTable


{-| A _left join_ preserves all the values in the first table and adds any key-matched
values from columns in the second table to it. Where both tables share common column
names, only those in the left (first) table are stored in the output.

    leftJoin ( table1, "Key" ) ( table2, "Key" )

would generate

```markdown
| Key | colLabelA | colLabelB | colLabelC | colLabelD |
| --- | --------- | --------- | --------- | --------- |
| k1  | a1        | b1        |           |           |
| k2  | a2        | b2        | c2        | d2        |
| k3  | a3        | b3        |           |           |
| k4  | a4        | b4        | c4        | d4        |
```

-}
leftJoin : ( Table, Heading ) -> ( Table, Heading ) -> Table
leftJoin ( t1, heading1 ) ( t2, heading2 ) =
    let
        keyCol colLabel =
            case ( Dict.get heading2 (getColumns t2), Dict.get colLabel (getColumns t2) ) of
                ( Just ks, Just vs ) ->
                    Dict.fromList (List.map2 Tuple.pair ks vs)

                _ ->
                    Dict.empty

        tableCol colLabel =
            List.map (\k -> Dict.get k (keyCol colLabel) |> Maybe.withDefault "")
                (Dict.get heading1 (getColumns t1) |> Maybe.withDefault [])

        leftInsert label2 columns =
            if Dict.member label2 columns then
                columns

            else
                columns |> Dict.insert label2 (tableCol label2)
    in
    List.foldl leftInsert (getColumns t1) (Dict.keys (getColumns t2)) |> toTable


{-| A _right join_ preserves all the values in the second table and adds any key-matched
values from columns in the first table to it. Where both tables share common column
names, only those in the right (second) table are stored in the output.

    rightJoin ( table1, "Key" ) ( table2, "Key" )

would generate

```markdown
| Key | colLabelA | colLabelB | colLabelC | colLabelD |
| --- | --------- | --------- | --------- | --------- |
| k2  | a2        | b2        | c2        | d2        |
| k4  | a4        | b4        | c4        | d4        |
| k6  |           |           | c6        | d6        |
| k8  |           |           | c8        | d8        |
```

-}
rightJoin : ( Table, Heading ) -> ( Table, Heading ) -> Table
rightJoin =
    flip leftJoin


{-| An 'inner join' will contain only key-matched rows that are present in both tables.

    innerJoin ( table1, "Key" ) ( table2, "Key" )

would generate

```markdown
| Key | colLabelA | colLabelB | colLabelC | colLabelD |
| --- | --------- | --------- | --------- | --------- |
| k2  | a2        | b2        | c2        | d2        |
| k4  | a4        | b4        | c4        | d4        |
```

-}
innerJoin : ( Table, Heading ) -> ( Table, Heading ) -> Table
innerJoin ( t1, heading1 ) ( t2, heading2 ) =
    let
        keyCol colLabel =
            case ( Dict.get heading2 (getColumns t2), Dict.get colLabel (getColumns t2) ) of
                ( Just ks, Just vs ) ->
                    Dict.fromList (List.map2 Tuple.pair ks vs)

                _ ->
                    Dict.empty

        tableCol colLabel =
            List.map (\k -> Dict.get k (keyCol colLabel) |> Maybe.withDefault "")
                (Dict.get heading1 (getColumns t1) |> Maybe.withDefault [])
    in
    List.foldl (\label2 -> Dict.insert label2 (tableCol label2)) (getColumns t1) (Dict.keys (getColumns t2))
        |> toTable
        |> filterRows heading2 (not << String.isEmpty)


{-| An _outer join_ contains all rows of both joined tables.

    outerJoin ( table1, "Key" ) ( table2, "Key" )

would generate

```markdown
| Key | colLabelA | colLabelB | colLabelC | colLabelD |
| --- | --------- | --------- | --------- | --------- |
| k1  | a1        | b1        |           |           |
| k2  | a2        | b2        | c2        | d2        |
| k3  | a3        | b3        |           |           |
| k4  | a4        | b4        | c4        | d4        |
| k6  |           |           | c6        | d6        |
| k8  |           |           | c8        | d8        |
```

-}
outerJoin : ( Table, Heading ) -> ( Table, Heading ) -> Table
outerJoin ( t1, heading1 ) ( t2, heading2 ) =
    let
        leftColumns =
            leftJoin ( t1, heading1 ) ( t2, heading2 ) |> getColumns

        rightTable =
            rightJoin ( t1, heading1 ) ( t2, heading2 )

        diff =
            filterRows heading2
                (\s -> Basics.not (List.member s (Dict.get heading1 leftColumns |> Maybe.withDefault [])))
                rightTable
    in
    Dict.map (\k v -> v ++ (Dict.get k (getColumns diff) |> Maybe.withDefault [])) leftColumns
        |> toTable



-- ------------------------------- Private


flip : (a -> b -> c) -> b -> a -> c
flip fn argB argA =
    fn argA argB


transpose : List (List a) -> List (List a)
transpose xss =
    let
        numCols =
            List.head >> Maybe.withDefault [] >> List.length
    in
    List.foldr (List.map2 (::)) (List.repeat (numCols xss) []) xss


columnsHead : Columns -> List ( Heading, Cell )
columnsHead columns =
    if List.foldl (max << List.length) 0 (Dict.values columns) == 0 then
        []

    else
        Dict.foldl (\k v -> (::) ( k, List.head v |> Maybe.withDefault "" )) [] columns


columnsTail : Columns -> Columns
columnsTail =
    Dict.map (always (List.drop 1))


toTable : Columns -> Table
toTable cols =
    Table { columns = cols }


getColumns : Table -> Columns
getColumns tbl =
    case tbl of
        Table cols ->
            .columns cols
