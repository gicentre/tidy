module Tidy exposing
    ( Table
    , fromCSV
    , tableSummary
    , melt
    , leftJoin
    , rightJoin
    , innerJoin
    , outerJoin
    , filterRows
    , filterColumns
    , toColumn
    , numColumn
    , strColumn
    , boolColumn
    )

{-| A collection of utilities for representing tabular data and reshaping them.


## Table Representation

@docs Table
@docs fromCSV
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


## Column coversion

@docs toColumn
@docs numColumn
@docs strColumn
@docs boolColumn

-}

import Dict exposing (Dict)
import Regex


{-| The main unit of data organsiation is the `Table`, which is a collection of
data columns each referenced by a column name. All table values are
[Strings](https://package.elm-lang.org/packages/elm/core/latest/String) but can be
converted into other types when necessary.
-}
type alias Table =
    Dict String (List String)


{-| Given a multi-line comma-separated string representing a table in the form:

    """colLabelA,colLabelB,colLabelC,etc.
       a1,b1,c1, etc.
       a2,b2,c2, etc.
       a3,b3,c3, etc.
       etc."""

Transform it into a dictonary of columns referenced by the column label (a `Table`):

```markdown
colLabelA → [a1, a2, a3, ...]
colLabelB → [b1, b2, b3, ...]
colLabelC → [c1, c2, c3, ...]
etc.
```

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

        addEntry xs =
            case xs of
                hd :: tl ->
                    Dict.insert (String.trim hd) (List.map String.trim tl)

                _ ->
                    identity
    in
    String.split "\n"
        >> List.filter (not << String.isEmpty)
        >> List.map (\s -> " " ++ s ++ " ")
        -- regex modified from https://stackoverflow.com/a/42535295 to account for whitespace between commas adjacent to quoted entries.
        -- For Elm parser approach consider https://gist.github.com/BrianHicks/165554b033eb797e3ed851964ecb3a38
        >> List.map (submatches "(?:,\\s*\"|^\")(\"\"|[\\w\\W]*?)(?=\"\\s*,|\"$)|(?:,(?!\")|^(?!\"))([^,]*?)(?=$|,)")
        >> transpose
        >> List.foldl addEntry Dict.empty


{-| Extract the values of a given column from a table. The type of values in the
column is determined by the given string conversion function. Any conversions that
fail are not included in the resulting list of values.

    imputeMissing : String -> Maybe Int
    imputeMissing val =
        case String.toInt val of
            Just n ->
                Just n

            Nothing ->
                Just 0

    myTable |> toColumn "count" imputeMissing

-}
toColumn : String -> (String -> Maybe a) -> Table -> List a
toColumn colName converter =
    Dict.get colName
        >> Maybe.withDefault []
        >> List.filterMap converter


{-| Extract the numeric values of a given column from a table. Any conversions that
fail are not included in the resulting list of values. This is a convenience function
equivalent calling [toColumn](#toColumn) providing the conversion function
[String.toFloat](https://package.elm-lang.org/packages/elm/core/latest/String#toFloat)

    myTable |> numColumn "year"

-}
numColumn : String -> Table -> List Float
numColumn colName =
    toColumn colName String.toFloat


{-| Extract the string values of a given column from a table.

    myTable |> strColumn "location"

-}
strColumn : String -> Table -> List String
strColumn colName =
    toColumn colName Just


{-| Extract Boolean values of a given column from a table. Any conversions that
fail are not included in the resulting list of values. Assumes that `True` values
can be represented by the case-insenstive strings `true`, `yes` and `1`, and that
`False` values can be represented by `false`, `no` and `0`. All other values will
be ignored and not provided in the resulting list.

      myTable |> toBool "isMarried"

-}
boolColumn : String -> Table -> List Bool
boolColumn colName =
    let
        toBool str =
            case String.toLower str of
                "true" ->
                    Just True

                "yes" ->
                    Just True

                "1" ->
                    Just True

                "false" ->
                    Just False

                "no" ->
                    Just False

                "0" ->
                    Just False

                _ ->
                    Nothing
    in
    toColumn colName toBool


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
            tbl |> Dict.keys |> addDividers

        divider =
            tbl |> Dict.keys |> List.map (always "-") |> addDividers

        values =
            Dict.values tbl
                |> List.map (List.take mx)
                |> transpose
                |> List.map (\ss -> addDividers ss ++ [ "\n" ])
                |> List.concat

        numTableRows =
            List.foldl (max << List.length) 0 (Dict.values tbl)

        continues =
            if numTableRows > mx then
                tbl |> Dict.keys |> List.map (always " : ") |> addDividers

            else
                []

        dimensions =
            [ "\n", String.fromInt numTableRows, " rows and ", String.fromInt (Dict.size tbl), " columns in total." ]
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

The first two parameters represent the names to be given to the column reference
(`year` in the example above) and variable column (`temperature` in the example above)
to be generated. The third is a list of the (columnName,columnReference)
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
melt : String -> String -> List ( String, String ) -> Table -> Table
melt columnName valueName colVars table =
    let
        columnLookup =
            Dict.fromList colVars

        emptyTable =
            table
                |> Dict.keys
                |> List.filter (\s -> not (List.member s (List.map Tuple.first colVars)))
                |> (++) [ columnName, valueName ]
                |> List.map (\label -> ( label, [] ))
                |> Dict.fromList

        newRow tbl =
            let
                meltCol ( oldLabel, val ) =
                    case Dict.get oldLabel columnLookup of
                        Just colRef ->
                            Just [ ( columnName, colRef ), ( valueName, val ) ]

                        Nothing ->
                            Nothing

                originalCol ( oldLabel, val ) =
                    case Dict.get oldLabel columnLookup of
                        Just colRef ->
                            Nothing

                        Nothing ->
                            Just ( oldLabel, val )

                originalCols =
                    tableHead tbl |> List.filterMap originalCol
            in
            tableHead tbl
                |> List.filterMap meltCol
                |> List.map (\x -> x ++ originalCols)

        addToColumn lbl tbl val =
            (Dict.get lbl tbl |> Maybe.withDefault []) ++ [ val ]

        addMeltedRows row tbl =
            List.foldl (\( lbl, val ) -> Dict.insert lbl (addToColumn lbl tbl val)) tbl row

        extractRows ( oldTable, newTable ) =
            case tableHead oldTable of
                [] ->
                    ( oldTable, newTable )

                row ->
                    extractRows ( tableTail oldTable, List.foldr addMeltedRows newTable (newRow oldTable) )
    in
    extractRows ( table, emptyTable ) |> Tuple.second


{-| Keep rows in the table where the values in the given column satisfy the given
test. The test should be a function that takes a string representing the cell value
and returns either `True` or `False` depending on whether the row containing that
value in the column should be retained.

    isWarm : String -> Bool
    isWarm s =
        case String.toFloat s of
            Just x ->
                x >= 10

            Nothing ->
                False

    warmCities =
        myTable |> filterRows "temperature" isWarm

-}
filterRows : String -> (String -> Bool) -> Table -> Table
filterRows columnName fn tbl =
    let
        predicate n val =
            if fn val then
                n

            else
                -1

        colValues =
            case Dict.get columnName tbl of
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
    Dict.map filterFromCol tbl


{-| Keep columns in the table whose names satisfy the given test. The test should
be a function that takes a string representing the column name and returns either
`True` or `False` depending on whether the column should be retained.

    myTable |> filterColumns ((==) "temperature2017")

-}
filterColumns : (String -> Bool) -> Table -> Table
filterColumns fn tbl =
    Dict.filter (\k _ -> fn k) tbl


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
leftJoin : ( Table, String ) -> ( Table, String ) -> Table
leftJoin ( t1, k1 ) ( t2, k2 ) =
    let
        keyCol colLabel =
            case ( Dict.get k2 t2, Dict.get colLabel t2 ) of
                ( Just ks, Just vs ) ->
                    Dict.fromList (List.map2 Tuple.pair ks vs)

                _ ->
                    Dict.empty

        tableCol colLabel =
            List.map (\k -> Dict.get k (keyCol colLabel) |> Maybe.withDefault "")
                (Dict.get k1 t1 |> Maybe.withDefault [])

        leftInsert label2 table =
            if Dict.member label2 table then
                table

            else
                Dict.insert label2 (tableCol label2) table
    in
    List.foldl leftInsert t1 (Dict.keys t2)


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
rightJoin : ( Table, String ) -> ( Table, String ) -> Table
rightJoin ( t1, k1 ) ( t2, k2 ) =
    leftJoin ( t2, k2 ) ( t1, k1 )


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
innerJoin : ( Table, String ) -> ( Table, String ) -> Table
innerJoin ( t1, k1 ) ( t2, k2 ) =
    let
        keyCol colLabel =
            case ( Dict.get k2 t2, Dict.get colLabel t2 ) of
                ( Just ks, Just vs ) ->
                    Dict.fromList (List.map2 Tuple.pair ks vs)

                _ ->
                    Dict.empty

        tableCol colLabel =
            List.map (\k -> Dict.get k (keyCol colLabel) |> Maybe.withDefault "")
                (Dict.get k1 t1 |> Maybe.withDefault [])
    in
    List.foldl (\label2 -> Dict.insert label2 (tableCol label2)) t1 (Dict.keys t2)
        |> filterRows k2 (not << String.isEmpty)


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
outerJoin : ( Table, String ) -> ( Table, String ) -> Table
outerJoin ( t1, k1 ) ( t2, k2 ) =
    let
        left =
            leftJoin ( t1, k1 ) ( t2, k2 )

        right =
            rightJoin ( t1, k1 ) ( t2, k2 )

        diff =
            filterRows k2 (\s -> Basics.not (List.member s (Dict.get k1 left |> Maybe.withDefault []))) right
    in
    Dict.map (\k v -> v ++ (Dict.get k diff |> Maybe.withDefault [])) left



-- ------------------------------- Private


transpose : List (List a) -> List (List a)
transpose listOfLists =
    let
        heads =
            List.filterMap List.head listOfLists

        tails =
            List.filterMap List.tail listOfLists
    in
    if List.length heads == 0 then
        []

    else if List.length heads == List.length listOfLists then
        heads :: transpose tails

    else
        []


tableHead : Table -> List ( String, String )
tableHead tbl =
    if List.foldl (max << List.length) 0 (Dict.values tbl) == 0 then
        []

    else
        Dict.foldl (\k v -> (::) ( k, List.head v |> Maybe.withDefault "" )) [] tbl


tableTail : Table -> Table
tableTail =
    Dict.map (always (List.drop 1))
