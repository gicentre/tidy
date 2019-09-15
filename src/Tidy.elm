module Tidy exposing
    ( Table
    , fromCSV
    , fromDelimited
    , fromGrid
    , fromGridRows
    , empty
    , insertRow
    , filterRows
    , renameColumn
    , insertColumn
    , insertColumnFromJson
    , insertIndexColumn
    , removeColumn
    , mapColumn
    , filterColumns
    , gather
    , spread
    , bisect
    , splitAt
    , headTail
    , disaggregate
    , transposeTable
    , leftJoin
    , rightJoin
    , innerJoin
    , outerJoin
    , leftDiff
    , rightDiff
    , tableSummary
    , columnNames
    , toCSV
    , toDelimited
    , numColumn
    , strColumn
    , booColumn
    , toColumn
    )

{-| Tidy and shape tabular data.

@docs Table


# Create

@docs fromCSV
@docs fromDelimited
@docs fromGrid
@docs fromGridRows
@docs empty


# Edit

@docs insertRow
@docs filterRows

@docs renameColumn
@docs insertColumn
@docs insertColumnFromJson
@docs insertIndexColumn
@docs removeColumn
@docs mapColumn
@docs filterColumns


# Tidy

Arranging _tidy data_ ([Wickham, 2014](https://www.jstatsoft.org/index.php/jss/article/view/v059i10/v59i10.pdf))
is a convention for organising tabular data such that columns represent distinct
_variables_ and rows represent _observations_. This isolates the semantic meaning
of items in any column independently of all others. The effect is to greatly
simplify data interchange and many data analytical functions.

Wickham identifies some common problems with data that are not in tidy format
("messy" data), each of which can be solved with a small number of simple operations:

  - Column headers are values not variable names (solved with [gather](#gather)).
  - Multiple variables are stored in the same column (solved with [bisect](#bisect)).
  - Variables arranged in rows as well as columns (solved with [spread](#spread)).
  - Multiple types of observational unit are stored in the same table (solved with [filterColumns](#filterColumns)).
  - The same observational unit is stored in multiple tables. (solved with joins such as [leftJoin](#leftJoin)).

@docs gather
@docs spread
@docs bisect
@docs splitAt
@docs headTail
@docs disaggregate
@docs transposeTable


# Join

Join two tables using a common key. While not specific to tidy data, joining tidy
tables is often more meaningful than joining messy ones. Joins often rely on the
existence of a 'key' column containing unique row identifiers. If tables to be
joined do not have such a key, they can be added with
[insertIndexColumn](#insertIndexColumn).

The examples below illustrate joining two input tables with shared key values
`k2` and `k4`:

```markdown
table1:

| Key1 | colA | colB |
| ---- | ---- | ---- |
| k1   | a1   | b1   |
| k2   | a2   | b2   |
| k3   | a3   | b3   |
| k4   | a4   | b4   |

table2:

| Key2 | colC | colD |
| ---- | ---- | ---- |
| k2   | c2   | d2   |
| k4   | c4   | d4   |
| k6   | c6   | d6   |
| k8   | c8   | d8   |
```

@docs leftJoin
@docs rightJoin
@docs innerJoin
@docs outerJoin
@docs leftDiff
@docs rightDiff


# Output

@docs tableSummary
@docs columnNames
@docs toCSV
@docs toDelimited


## Column output

@docs numColumn
@docs strColumn
@docs booColumn
@docs toColumn

-}

import CSVParser
import Dict exposing (Dict)
import Json.Decode
import Regex
import Set


{-| The basic organisational unit for tabular data. Each column in a table has a
unique name by which it may be referenced. Table cell values are represented as
Strings, but can be converted to other types via column output functions
(e.g. [numColumn](#numColumn)).
-}
type
    Table
    -- Opaque type
    = Table { columns : Columns }


{-| Split a named column (first parameter) into two with a bisecting function
(second parameter). The third parameter should be the names to give the two new
columns, which are inserted into the table replacing the original bisected column.

For example, given a table

```markdown
| row | col |   z |
| --: | --: | --- |
|   0 |   0 | z00 |
|   0 |   1 | z01 |
|   0 |   2 | z02 |
|   1 |   0 | z10 |
|   1 |   1 | z11 |
|   1 |   2 | z12 |
```

bisecting it with

    bisect "z"
        (\z ->
            ( String.left 2 z
            , String.left 1 z ++ String.right 1 z
            )
        )
        ( "zr", "zc" )

produces the table

```markdown
| row | col | zr | zc |
| --: | --: | -- | -- |
|   0 |   0 | z0 | z0 |
|   0 |   1 | z0 | z1 |
|   0 |   2 | z0 | z2 |
|   1 |   0 | z1 | z0 |
|   1 |   1 | z1 | z1 |
|   1 |   2 | z1 | z2 |
```

If the column to be bisected is not found, the original table is returned.

For more sophisticated disaggregation, such as splitting a column into more than
two new ones, consider [disaggregate](#disaggregate).

-}
bisect : String -> (String -> ( String, String )) -> ( String, String ) -> Table -> Table
bisect heading bisector ( newHeading1, newHeading2 ) tbl =
    case tableColumns tbl |> getColumn heading of
        Just colValues ->
            let
                newCols =
                    List.map bisector colValues |> List.unzip
            in
            tbl
                |> insertColumn newHeading1 (Tuple.first newCols)
                |> insertColumn newHeading2 (Tuple.second newCols)
                |> removeColumn heading

        Nothing ->
            tbl


{-| Extract Boolean values of a given column from a table. Assumes that `True`
values can be represented by the case-insensitive strings `true`, `yes` and `1`
while all other values are assumed to be false.

    dataColumn =
        myTable |> booColumn "isMarried"

-}
booColumn : String -> Table -> List Bool
booColumn heading =
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


{-| Provide a list of column names for the given table.
-}
columnNames : Table -> List String
columnNames =
    tableColumns >> Dict.keys >> List.map Tuple.second


{-| Disaggregate the values in a given column (first parameter) according to a
regular expression (second parameter). The names to give to the new disaggregated
columns are provided in the third parameter. The number of groups returned by the
regular expression should match the number of new column names. For example,
to disaggregate `diagnosisCohort` in the following table:

```markdown
| diagnosisCohort | numCases |
| --------------- | -------: |
| new_sp_m014     |       52 |
| new_sp_m1524    |      228 |
| new_sp_f014     |       35 |
| new_sp_f1524    |      180 |
| new_sn_m014     |        9 |
| new_sn_m1524    |       97 |
| new_sn_f014     |       11 |
| new_sn_f1524    |       64 |
```

    disaggregate "diagnosisCohort"
        "new_?(.*)_(.)(.*)"
        [ "diagnosis", "gender", "age" ]

produces a new table:

```markdown
| numCases | diagnosis | gender | age  |
| -------: | --------- | ------ | ---- |
|       52 |        sp |      m | 014  |
|      228 |        sp |      m | 1524 |
|       35 |        sp |      f | 014  |
|      180 |        sp |      f | 1524 |
|        9 |        sn |      m | 014  |
|       97 |        sn |      m | 1524 |
|       11 |        sn |      f | 014  |
|       64 |        sn |      f | 1524 |
```

If the colunn to disaggregate cannot be found, the original table is returned.

-}
disaggregate : String -> String -> List String -> Table -> Table
disaggregate heading regex newHeadings tbl =
    let
        matchedGroups =
            Regex.find
                (Regex.fromString regex |> Maybe.withDefault Regex.never)
                >> List.concatMap .submatches
                >> List.filterMap identity

        newCols cells =
            List.map matchedGroups cells
                |> transpose
                |> zip newHeadings
    in
    case tableColumns tbl |> getColumn heading of
        Just colValues ->
            List.foldl (\( newHead, newCol ) t -> insertColumn newHead newCol t) tbl (newCols colValues)
                |> removeColumn heading

        Nothing ->
            tbl


{-| Create an empty table. Useful if table items are to be added programatically
with `insertRow` and `insertColumn`.
-}
empty : Table
empty =
    toTable Dict.empty


{-| Keep columns in the table whose names satisfy the given test. The test should
be a function that takes a column heading and returns either `True` or `False`
depending on whether the column should be retained.

    newTable =
        myTable
            |> filterColumns (\s -> String.left 11 s == "temperature")

-}
filterColumns : (String -> Bool) -> Table -> Table
filterColumns fn =
    tableColumns
        >> Dict.filter (\( _, colHeading ) _ -> fn colHeading)
        >> compactIndices 0
        >> toTable


{-| Keep rows in the table where the values in the given column satisfy the given
test. The test should be a function that takes a cell value and returns either
`True` or `False` depending on whether the row containing that value in the column
should be retained.

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
            case getColumn columnName (tableColumns tbl) of
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
    tbl |> tableColumns |> Dict.map filterFromCol |> toTable


{-| Create a table from a multi-line comma-separated string. For example

      myTable =
          """colA,colB,colC
      a1,b1,c1
      a2,b2,c2
      a3,b3,c3"""
              |> fromCSV

-}
fromCSV : String -> Table
fromCSV =
    fromDelimited ','


{-| Create a table from a multi-line string where values are separated by the
given delimiter (first parameter). For example, to process a tab-delimited values
file (TSV):

    myTable =
        """colA colB colC
    a1  b1  c1
    a2  b2  c2
    a3  b3  c3"""
            |> fromDelimited '\t'

-}
fromDelimited : Char -> String -> Table
fromDelimited delimiter =
    let
        addEntry xs =
            case xs of
                ( n, hd :: tl ) ->
                    Dict.insert ( n, String.trim hd ) (List.map String.trim tl)

                _ ->
                    identity
    in
    CSVParser.parseDelimited delimiter
        >> transpose
        >> List.indexedMap Tuple.pair
        >> List.foldl addEntry Dict.empty
        >> toTable


{-| Transform multi-line string input values in the form:

    """
       z00,z01,z02,z03, etc.
       z10,z11,z12,z13, etc.
       z20,z21,z22,c23, etc.
       z30,z31,z32,c33, etc.
       etc."""

into a tidy table in the form:

```markdown
| row | col |   z |
| --: | --: | --: |
|   0 |   0 | z00 |
|   0 |   1 | z01 |
|   0 |   2 | z02 |
|   0 |   3 | z03 |
|   1 |   0 | z10 |
|   1 |   1 | z11 |
|     |     |     |
```

Values between commas are outer-trimmed of whitespace unless enclosed in quotes
and entirely blank lines are ignored. Input can be ragged with different numbers
of columns in each row.

Note the common convention that in grids, the origin (row 0, col 0) is at the top-left,
whereas in Cartesian coordinate systems the origin (x=0, y=0) is at the bottom-left.
You may therefore wish to reverse the order of row values in the input string if
you are mapping onto a Cartesian coordinate system. For example,

    """z00,z01,z02,z03
    z10,z11,z12,z13
    z20,z21,z22,c23
    z30,z31,z32,c33"""
        |> String.split "\n"
        |> List.reverse
        |> String.join "\n"
        |> fromGrid
        |> renameColumn "row" "y"
        |> renameColumn "col" "x"

-}
fromGrid : String -> Table
fromGrid =
    String.lines
        >> List.filter (not << String.isEmpty)
        >> List.map (String.split ",")
        -- >> List.filter (not << String.isEmpty))
        >> fromGridRows


{-| Transform list of input string lists in the form:

    [ [z00, z01, z02, z03, ...]
    , [z10, z11, z12, z13, ...]
    , [z20, z21, z22, c23, ...]
    , [z30, z31, z32, c33, ...]
    , [...]
    ]

into a tidy table in the form:

```markdown
| row | col | z   |
| --: | --: | --- |
|   0 |   0 | z00 |
|   0 |   1 | z01 |
|   0 |   2 | z02 |
|   0 |   3 | z03 |
|   1 |   0 | z10 |
|   1 |   1 | z11 |
|     |     |     |
```

Input can be ragged with different numbers of columns in each row. Entirely empty
rows (i.e. `[]`) are ignored, but cells with empty strings (e.g. `[""]`) are captured.

As with [fromGrid](#fromGrid), you may wish to reverse the input row order if you
are mapping onto a Cartesian coordinate system.

-}
fromGridRows : List (List String) -> Table
fromGridRows =
    let
        updateCol v maybeCol =
            case maybeCol of
                Nothing ->
                    Just [ v ]

                Just col ->
                    Just (col ++ [ v ])

        addGridCell ( r, c, z ) =
            Dict.update ( 0, "row" ) (updateCol r)
                >> Dict.update ( 1, "col" ) (updateCol c)
                >> Dict.update ( 2, "z" ) (updateCol z)
    in
    List.filter (not << List.isEmpty)
        >> List.indexedMap
            (\r -> List.indexedMap (\c z -> ( String.fromInt r, String.fromInt c, String.trim z )))
        >> List.concat
        >> List.foldl addGridCell Dict.empty
        >> toTable


{-| Combine several columns that represent the same variable into two columns, one
referencing the original column, the other the values of the variable. For example,
the following messy table

```markdown
| location  | temperature2017 | temperature2018 |
| --------- | --------------: | --------------: |
| Bristol   |              12 |              14 |
| Sheffield |              11 |              13 |
| Glasgow   |               8 |               9 |
| Aberdeen  |                 |               7 |
```

can be gathered to create a tidy table:

```markdown
| location  | year | temperature |
| --------- | ---- | ----------: |
| Bristol   | 2017 |          12 |
| Bristol   | 2018 |          14 |
| Sheffield | 2017 |          11 |
| Sheffield | 2018 |          13 |
| Glasgow   | 2017 |           8 |
| Glasgow   | 2018 |           9 |
| Aberdeen  | 2018 |           7 |
```

The first two parameters represent the names to be given to the new reference column
(`year` in the example above) and variable column (`temperature` in the example
above). The third is a list of the (columnName,columnReference) to be gathered
(e.g. `[ ("temperature2017", "2017"), ("temperature2018", "2018") ]` above).

Only non-empty cell values in the variable column are gathered (e.g. note that only
`Aberdeen, 2018, 7` is gathered with no entry for 2017.)

If none of the `columnName`s in the third parameter is found in the table, an empty
table is returned.

For cases where more than one set of columns needs to be gathered, you can combine
three stages: (a) gather all columns, adding a column group id; (b) bisect column
group id and column reference; (c) spread the bisected columns. For example:

    """flowID,originLong,originLat,destLong,destLat
       1,-71.9,41.8,-71.5,41.6
       2,-80.5,34.9,-97.6,30.2
       3,-92.1,37.0,-86.8,43.6"""
        |> fromCSV
        |> gather "odCoordType"
            "value"
            [ ( "originLong", "oLong" )
            , ( "originLat", "oLat" )
            , ( "destLong", "dLong" )
            , ( "destLat", "dLat" )
            ]
        |> bisect "odCoordType" headTail ( "od", "coordType" )
        |> spread "coordType" "value"

creates the table

```markdown
| flowID | od |  Long |  Lat |
| -----: | -- | ----- | ---- |
|      1 |  o | -71.9 | 41.8 |
|      1 |  d | -71.5 | 41.6 |
|      2 |  o | -80.5 | 34.9 |
|      2 |  d | -97.6 | 30.2 |
|      3 |  o | -92.1 | 37.0 |
|      3 |  d | -86.8 | 43.6 |
```

-}
gather : String -> String -> List ( String, String ) -> Table -> Table
gather columnName valueName colVars table =
    let
        colToVarLookup : Dict String Cell
        colToVarLookup =
            Dict.fromList colVars

        numCols =
            Dict.size (tableColumns table)

        emptyColumns : Columns
        emptyColumns =
            table
                |> tableColumns
                |> Dict.keys
                |> List.filter (\( _, s ) -> not (List.member s (List.map Tuple.first colVars)))
                |> (++) [ ( numCols, columnName ), ( numCols + 1, valueName ) ]
                |> List.map (\label -> ( label, [] ))
                |> Dict.fromList

        newRows : Columns -> List (List ( Heading, Cell ))
        newRows columns =
            let
                gatherCol ( ( _, oldHeading ), val ) =
                    case Dict.get oldHeading colToVarLookup of
                        Just colRef ->
                            Just [ ( ( numCols, columnName ), colRef ), ( ( numCols + 1, valueName ), val ) ]

                        Nothing ->
                            Nothing

                ungatheredCol ( ( n, oldHeading ), val ) =
                    case Dict.get oldHeading colToVarLookup of
                        Just colRef ->
                            Nothing

                        Nothing ->
                            Just ( ( n, oldHeading ), val )

                ungatheredCols =
                    columns |> columnsHead |> List.filterMap ungatheredCol
            in
            columns
                |> columnsHead
                |> List.filterMap gatherCol
                |> List.map (\x -> x ++ ungatheredCols)

        addToColumn : Heading -> Columns -> Cell -> List Cell
        addToColumn heading columns val =
            (Dict.get heading columns |> Maybe.withDefault []) ++ [ val ]

        addGatheredRows : List ( Heading, Cell ) -> Columns -> Columns
        addGatheredRows row columns =
            List.foldl (\( heading, val ) -> Dict.insert heading (addToColumn heading columns val)) columns row

        extractRows : ( Columns, Columns ) -> ( Columns, Columns )
        extractRows ( oldColumns, newColumns ) =
            case columnsHead oldColumns of
                [] ->
                    ( oldColumns, newColumns )

                row ->
                    extractRows ( columnsTail oldColumns, List.foldr addGatheredRows newColumns (newRows oldColumns) )

        gatheredTable =
            extractRows ( tableColumns table, emptyColumns )
                |> Tuple.second
                |> compactIndices 0
                |> toTable
                |> filterRows valueName ((/=) "")
    in
    if numRows gatheredTable == 0 then
        empty

    else
        gatheredTable


{-| Convenience function for splitting a string into its first (head) and remaining
(tail) characters. e.g. `headTail "tidy" == ("t","idy")`. Equivalent to `splitAt 1`.
Useful when using [bisect](#bisect) to split column values into one column of heads
and another of tails.
-}
headTail : String -> ( String, String )
headTail str =
    ( String.left 1 str, String.dropLeft 1 str )


{-| An _inner join_ will contain only key-matched rows that are present in both
tables. The first parameter is the name to give the new key-matched column,
replacing the separate key names in the two tables. Where both tables share a
common column name, the one in the first table is prioritised.

    innerJoin "Key" ( table1, "Key1" ) ( table2, "Key2" )

would generate

```markdown
| Key | colA | colB | colC | colD |
| --- | ---- | ---- | ---- | ---- |
| k2  | a2   | b2   | c2   | d2   |
| k4  | a4   | b4   | c4   | d4   |
```

If one or both of the key columns are not found, this produces an empty table.

-}
innerJoin : String -> ( Table, String ) -> ( Table, String ) -> Table
innerJoin keyName ( oldT1, key1 ) ( oldT2, key2 ) =
    -- Check that the named keys are present in both tables
    if not <| memberColumns key1 (tableColumns oldT1) && memberColumns key2 (tableColumns oldT2) then
        empty

    else
        let
            t1 =
                oldT1 |> renameColumn key1 keyName

            t2 =
                oldT2 |> renameColumn key2 keyName

            lJoin =
                leftJoin ( t1, keyName ) ( t2, keyName )

            t2Keys =
                getColumn keyName (tableColumns t2) |> Maybe.withDefault []
        in
        leftJoin ( t1, keyName ) ( t2, keyName )
            |> filterRows keyName (\s -> List.member s t2Keys)


{-| Add a column of data to a table. The first parameter is the name to give the
column. The second is a list of column values. If the table already has a column
with this name, it will get replaced with the given data. To ensure table rows are
always aligned, if the table is not empty, the column values are padded / truncated
to match the number of rows in the table.
-}
insertColumn : String -> List String -> Table -> Table
insertColumn heading colValues tbl =
    insertColumnAt (Dict.size (tableColumns tbl)) heading colValues (tableColumns tbl)
        -- Need to compact indices in case the additional column replaces existing one.
        |> compactIndices 0
        |> toTable


{-| Add a column of data extracted from a JSON string onto a table. The first parameter
is the name of the JSON object containing the data values to add. This will become
the name of the column in the table. The second is a list of JSON object names that
define the path to the column object. This can be an empty list if the object is
in an array at the root of the JSON. The third parameter is the JSON string to parse
and the fourth the table to which a new column will be added. If there is a problem
finding the column object, the original table is returned.

For example,

    json =
        """[
      { "person": "John Smith", "treatment": "b", "result": 2 },
      { "person": "Jane Doe", "treatment": "a", "result": 16 },
      { "person": "Jane Doe", "treatment": "b", "result": 11 },
      { "person": "Mary Johnson", "treatment": "a", "result": 3 },
      { "person": "Mary Johnson", "treatment": "b", "result": 1 }
    ]"""

    table =
        empty
            |> insertColumnFromJson "person" [] json
            |> insertColumnFromJson "treatment" [] json
            |> insertColumnFromJson "result" [] json

would generate a table

```markdown
| person       | treatment | result |
| ------------ | --------- | -----: |
| John Smith   | b         |      2 |
| Jane Doe     | a         |     16 |
| Jane Doe     | b         |     11 |
| Mary Johnson | a         |      3 |
| Mary Johnson | b         |      1 |
```

-}
insertColumnFromJson : String -> List String -> String -> Table -> Table
insertColumnFromJson key path json =
    let
        extract keys jsVal values =
            case keys of
                hd :: tl ->
                    case jsVal of
                        JsArray list ->
                            List.foldl (extract keys) [] list ++ values

                        -- We need to match the current key with an object name
                        JsObject object ->
                            case Dict.get hd object of
                                Just matchedJsVal ->
                                    extract tl matchedJsVal values

                                Nothing ->
                                    []

                        _ ->
                            []

                [] ->
                    case jsVal of
                        JsInt num ->
                            String.fromInt num :: values

                        JsFloat num ->
                            String.fromFloat num :: values

                        JsString s ->
                            s :: values

                        _ ->
                            values
    in
    case Json.Decode.decodeString jsValDecoder json of
        Ok jsVal ->
            insertColumn key
                (extract (path ++ [ key ]) jsVal []
                    |> List.reverse
                )

        Err msg ->
            identity


{-| Add an index column to a table. The first parameter is the name to give the
new column containing index values. The second is a prefix to add to each index
value, useful for giving different tables different index values (or use `""` for
no prefix). If the table already has a column with this name, it will be replaced
with this index column.

Creating an index column can be useful when joining tables with keys that you wish
to guarantee are unique for each row. For example, to combine the rows of two
tables `table1` and `table2`, but which may contain repeated values:

    outerJoin "key"
        ( insertIndexColumn "key" "t1" table1, "key" )
        ( insertIndexColumn "key" "t2" table2, "key" )

-}
insertIndexColumn : String -> String -> Table -> Table
insertIndexColumn heading prefix tbl =
    let
        prefixFull =
            if prefix == "" then
                ""

            else
                prefix ++ "_"

        indices =
            List.range 1 (numRows tbl)
                |> List.map
                    (\r ->
                        prefixFull
                            ++ String.padLeft
                                (tbl |> numRows |> String.fromInt |> String.length)
                                '0'
                                (String.fromInt r)
                    )
    in
    insertColumnAt -1 heading indices (tableColumns tbl)
        -- Need to compact indices in case the additional column replaces existing one.
        |> compactIndices 0
        |> toTable


{-| Add a row of values to a table. The new values are represented by a list of
`(columnName,columnValue)` tuples. If the table being appended is not empty, the
column names should correspond to existing columns in the table or they will be
ignored. Any unspecified columns will have an empty string value inserted.
-}
insertRow : List ( String, String ) -> Table -> Table
insertRow namedCells tbl =
    if tableColumns tbl == Dict.empty then
        namedCells
            |> List.indexedMap (\n ( colHead, cell ) -> ( ( n, colHead ), [ cell ] ))
            |> Dict.fromList
            |> toTable

    else
        -- To guarantee we don't misalign columns, we always add a complete set of
        -- column values, even if they are not all provided as parameters.
        let
            newValues =
                Dict.fromList namedCells

            newCell colHead =
                Dict.get colHead newValues |> Maybe.withDefault ""
        in
        tbl
            |> tableColumns
            |> Dict.map (\( _, colHead ) colValues -> colValues ++ [ newCell colHead ])
            |> toTable


{-| Provides a table of all the rows in the first table that do not occur in any
key-matched rows in the second table.

    leftDiff ( table1, "Key1" ) ( table2, "Key2" )

would generate

```markdown
| Key1 | colA | colB |
| ---- | ---- | ---- |
| k1   | a1   | b1   |
| k3   | a3   | b3   |
```

If the first key is not found, an empty table is returned, if the second key is
not found, the first table is returned.

-}
leftDiff : ( Table, String ) -> ( Table, String ) -> Table
leftDiff ( t1, key1 ) ( t2, key2 ) =
    if not <| memberColumns key1 (tableColumns t1) then
        empty

    else
        let
            t2Keys =
                getColumn key2 (tableColumns t2) |> Maybe.withDefault []
        in
        filterRows key1 (\s -> not <| List.member s t2Keys) t1


{-| A _left join_ preserves all the values in the first table and adds any key-matched
values from columns in the second table to it. Where both tables share common column
names, including key columns, only those in the left (first) table are stored in the output.

    leftJoin ( table1, "Key1" ) ( table2, "Key2" )

would generate

```markdown
| Key1 | colA | colB | Key2 | colC | colD |
| ---- | ---- | ---- | ---- | ---- | ---- |
| k1   | a1   | b1   |      |      |      |
| k2   | a2   | b2   | k2   | c2   | d2   |
| k3   | a3   | b3   |      |      |      |
| k4   | a4   | b4   | k4   | c4   | d4   |
```

If one or both of the key columns are not found, the left table is returned.

-}
leftJoin : ( Table, String ) -> ( Table, String ) -> Table
leftJoin ( t1, key1 ) ( t2, key2 ) =
    -- Check that the named keys are present in both tables
    if not <| memberColumns key1 (tableColumns t1) && memberColumns key2 (tableColumns t2) then
        t1

    else
        let
            keyCol colLabel =
                case ( getColumn key2 (tableColumns t2), getColumn colLabel (tableColumns t2) ) of
                    ( Just ks, Just vs ) ->
                        Dict.fromList (zip ks vs)

                    _ ->
                        Dict.empty

            tableCol colLabel =
                List.map (\colHead -> Dict.get colHead (keyCol colLabel) |> Maybe.withDefault "")
                    (getColumn key1 (tableColumns t1) |> Maybe.withDefault [])

            leftInsert ( n, label2 ) columns =
                if memberColumns label2 columns then
                    columns

                else
                    columns |> Dict.insert ( n, label2 ) (tableCol label2)
        in
        List.foldl leftInsert (tableColumns t1) (Dict.keys (tableColumns t2 |> compactIndices (Dict.size (tableColumns t1))))
            |> compactIndices 0
            |> toTable


{-| Transform the contents of the given column (first parameter) with a mapping
function (second parameter). For example

    newTable =
        mapColumn "myColumnHeading" impute myTable

    impute val =
        if val == "" then
            "0"

        else
            val

If the column name is not found, the original table is returned.

-}
mapColumn : String -> (String -> String) -> Table -> Table
mapColumn heading fn tbl =
    case tableColumns tbl |> getColumn heading of
        Just colValues ->
            tbl |> insertColumn heading (List.map fn colValues)

        Nothing ->
            tbl


{-| Extract the numeric values of a given column from a table. Any conversions
that fail, including missing values in the table are converted into zeros. If
you wish to handle missing data / failed conversions in a different way, use
[toColumn](#toColumn) instead, providing a custom converter function.

    dataColumn =
        myTable |> numColumn "year"

-}
numColumn : String -> Table -> List Float
numColumn heading =
    toColumn heading (String.toFloat >> Maybe.withDefault 0)


{-| An _outer join_ contains all rows of both joined tables. The first parameter
is the name to give the new key-matched column, replacing the separate key names
in the two tables.

    outerJoin "Key" ( table1, "Key1" ) ( table2, "Key2" )

would generate

```markdown
| Key | colA | colB | colC | colD |
| --- | ---- | ---- | ---- | ---- |
| k1  | a1   | b1   |      |      |
| k2  | a2   | b2   | c2   | d2   |
| k3  | a3   | b3   |      |      |
| k4  | a4   | b4   | c4   | d4   |
| k6  |      |      | c6   | d6   |
| k8  |      |      | c8   | d8   |
```

If one or both of the key columns are not found, this produces an empty table.

-}
outerJoin : String -> ( Table, String ) -> ( Table, String ) -> Table
outerJoin keyName ( oldT1, key1 ) ( oldT2, key2 ) =
    -- TODO: What to do when tables have some common column names?
    if not <| memberColumns key1 (tableColumns oldT1) && memberColumns key2 (tableColumns oldT2) then
        empty

    else
        let
            t1 =
                oldT1 |> renameColumn key1 keyName

            t2 =
                oldT2 |> renameColumn key2 keyName

            leftColumns =
                leftJoin ( t1, keyName ) ( t2, keyName ) |> tableColumns

            rightTable =
                rightJoin ( t1, keyName ) ( t2, keyName )

            diff =
                filterRows keyName
                    (\s -> not <| List.member s (getColumn keyName leftColumns |> Maybe.withDefault []))
                    rightTable
        in
        Dict.map (\( n, k ) v -> v ++ (getColumn k (tableColumns diff) |> Maybe.withDefault [])) leftColumns
            |> toTable


{-| Remove a column with the given name from a table. If the column is not present
in the table, the original table is returned.
-}
removeColumn : String -> Table -> Table
removeColumn colName =
    tableColumns >> remove colName >> compactIndices 0 >> toTable


{-| Rename the given column (first parameter) with a new name (second parameter).
If the new column name matches an existing one, the existing one will be replaced
by the renamed column.
-}
renameColumn : String -> String -> Table -> Table
renameColumn oldName newName =
    tableColumns
        >> Dict.foldl
            (\( n, colName ) ->
                if colName == oldName then
                    Dict.insert ( n, newName )

                else
                    Dict.insert ( n, colName )
            )
            Dict.empty
        >> toTable


{-| Provides a table of all the rows in the second table that do not occur in any
key-matched rows in the first table.

    rightDiff ( table1, "Key1" ) ( table2, "Key2" )

would generate

```markdown
| Key2 | colC | colD |
| ---- | ---- | ---- |
| k6   | c6   | d6   |
| k8   | c8   | d8   |
```

If the first key is not found, the second table is returned, if the second key is
not found, an empty table is returned.

-}
rightDiff : ( Table, String ) -> ( Table, String ) -> Table
rightDiff =
    flip leftDiff


{-| A _right join_ preserves all the values in the second table and adds any
key-matched values from columns in the first table to it. Where both tables share
common column names, including key columns, only those in the right (second) table
are stored in the output.

    rightJoin ( table1, "Key1" ) ( table2, "Key2" )

would generate

```markdown
| Key2 | colC | colD | Key1 | colA | colB |
| ---- | ---- | ---- | ---- | ---- | ---- |
| k2   | c2   | d2   | k2   | a2   | b2   |
| k4   | c4   | d4   | k4   | a4   | b4   |
| k6   | c6   | d6   |      |      |      |
| k8   | c8   | d8   |      |      |      |
```

If one or both of the key columns are not found, the right table is returned.

-}
rightJoin : ( Table, String ) -> ( Table, String ) -> Table
rightJoin =
    flip leftJoin


{-| Convenience function for splitting a string (second parameter) at the given
position (first parameter).

    splitAt 4 "tidyString" == ( "tidy", "String" )

If the first parameter is negative, the position is counted from the right rather
than left.

    splitAt -4 "temperature2019" == ( "temperature", "2019" )

Useful when using [bisect](#bisect) to split column values in two.

-}
splitAt : Int -> String -> ( String, String )
splitAt n s =
    if n < 0 then
        ( String.left (String.length s + n) s, String.dropLeft (String.length s + n) s )

    else
        ( String.left n s, String.dropLeft n s )


{-| The inverse of [gather](#gather), spreading a pair of columns rotates values
to separate columns (like a _pivot_ in a spreadsheet). This is useful if different
variables are stored in separate rows of the same column. For example, the following
table contains two different variables in the `temperature` column:

```markdown
| location  | year | readingType | temperature |
| --------- | ---- | ----------- | ----------: |
| Bristol   | 2018 | minTemp     |           3 |
| Bristol   | 2018 | maxTemp     |          27 |
| Sheffield | 2018 | minTemp     |          -2 |
| Sheffield | 2018 | maxTemp     |          26 |
| Glasgow   | 2018 | minTemp     |         -10 |
| Glasgow   | 2018 | maxTemp     |          23 |
| Aberdeen  | 2018 | maxTemp     |          14 |
```

We can _spread_ the temperatures into separate columns reflecting their distinct
meanings, generating the table:

```markdown
| location  | year | minTemp | maxTemp |
| --------- | ---- | ------: | ------: |
| Bristol   | 2018 |       3 |      27 |
| Sheffield | 2018 |      -2 |      26 |
| Glasgow   | 2018 |     -10 |      23 |
| Aberdeen  | 2018 |         |      14 |
```

The first parameter is the name of the column containing the values that will form
the new spread column names (`readingType` above). The second parameter is the name
of the column containing the values to be inserted in each new column (`temperature`
above).

Missing rows (e.g. `Aberdeen, 2018, minTemp` above) are rotated as empty strings
in the spread column. If either of the columns to spread is not found, the original
table is returned.

-}
spread : String -> String -> Table -> Table
spread columnName valueName tbl =
    -- Check that both spread columms are present
    if not <| memberColumns columnName (tableColumns tbl) && memberColumns valueName (tableColumns tbl) then
        tbl

    else
        let
            -- Hash for each row excluding columnName and valueName columns
            nonSpreadHashes =
                tbl
                    |> filterColumns (\c -> c /= columnName && c /= valueName)
                    |> tableColumns
                    |> Dict.values
                    |> transpose
                    |> List.map String.concat

            rowHashlookup =
                zip (List.map2 (++) (strColumn columnName tbl) nonSpreadHashes)
                    (tbl |> tableColumns |> getColumn valueName |> Maybe.withDefault [])
                    |> Dict.fromList

            -- Hash for each columnName x rowHash combination (Cartesian product)
            cpHashes =
                nonSpreadHashes
                    |> unique
                    |> List.concatMap (\x -> List.map (\y -> ( y, y ++ x )) (tbl |> strColumn columnName |> unique))

            cpValues =
                List.map (\( _, hash ) -> Dict.get hash rowHashlookup |> Maybe.withDefault "")
                    cpHashes

            cpTable =
                empty
                    |> insertColumn columnName (List.map Tuple.first cpHashes)
                    |> insertColumn valueName cpValues

            newColumns =
                tbl
                    |> strColumn columnName
                    |> unique
                    |> List.map
                        (\nc -> ( nc, cpTable |> filterRows columnName ((==) nc) |> strColumn valueName ))

            newTable =
                tbl
                    |> filterColumns (\c -> c /= columnName && c /= valueName)
                    |> tableColumns
                    |> Dict.map (\( _, heading ) colVals -> List.map (\v -> ( heading, v )) colVals)
                    |> Dict.values
                    |> transpose
                    |> unique
                    |> List.foldl insertRow empty
        in
        List.foldl (\( nc, nvals ) t -> insertColumn nc nvals t) newTable newColumns


{-| Extract the string values of a given column from a table. Missing values in
the table are represented as empty strings. If you wish to handle missing values
in a different way, use [toColumn](#toColumn) instead, providing a custom converter
function.

    dataColumn =
        myTable |> strColumn "cityName"

-}
strColumn : String -> Table -> List String
strColumn heading =
    toColumn heading identity


{-| Provide a textual description of a table, configurable to show a given number
of table rows. If the number of rows to show is negative, all rows are output.
This is designed primarily to generate markdown output, but is interpretable as
raw text.
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
            --tbl |> tableColumns |> Dict.keys |> List.map (\( n, s ) -> String.fromInt n ++ "," ++ s) |> addDividers
            tbl |> tableColumns |> Dict.keys |> List.map Tuple.second |> addDividers

        divider =
            tbl |> tableColumns |> Dict.keys |> List.map (always "-") |> addDividers

        values =
            tbl
                |> tableColumns
                |> Dict.values
                |> List.map (List.take mx)
                |> transpose
                |> List.map (\ss -> addDividers ss ++ [ "\n" ])
                |> List.concat

        numTableRows =
            numRows tbl

        continues =
            if numTableRows > mx then
                tbl |> tableColumns |> Dict.keys |> List.map (always " : ") |> addDividers

            else
                []

        dimensions =
            [ "\n", String.fromInt numTableRows, " rows and ", String.fromInt (Dict.size (tableColumns tbl)), " columns in total." ]
    in
    [ headings, [ "\n" ], divider, [ "\n" ], values, continues, dimensions ]
        |> List.concat


{-| Extract the values of the column with the given name (first parameter) from a
table. The type of values in the column is determined by the given cell conversion
function. The converter function should handle cases of missing data in the table
(e.g. empty strings) as well as failed conversions (e.g. attempts to convert text
into a number).

    imputeMissing : String -> Int
    imputeMissing =
        String.toFloat >> Maybe.withDefault 0

    dataColumn =
        myTable |> toColumn "count" imputeMissing

-}
toColumn : String -> (String -> a) -> Table -> List a
toColumn heading converter =
    tableColumns
        >> getColumn heading
        >> Maybe.withDefault []
        >> List.map converter


{-| Provide a CSV (comma-separated values) format version of a table. Can be useful
for applications that need to save a table as a file.
-}
toCSV : Table -> String
toCSV =
    toDelimited ","


{-| Provide text containing table values separated by the given delimiter (first parameter).
Can be useful for applications that need to save a table as a file. For example,
to create tab-delimited (TSV) text representing a table for later saving as a file:

    toDelimited '\t' myTable

-}
toDelimited : String -> Table -> String
toDelimited delimiter tbl =
    let
        headings =
            tbl
                |> tableColumns
                |> Dict.keys
                |> List.map Tuple.second
                |> List.intersperse delimiter
                |> String.concat

        values =
            tbl
                |> tableColumns
                |> Dict.values
                |> transpose
                |> List.map (\ss -> List.intersperse delimiter ss ++ [ "\n" ])
                |> List.concat
                |> String.concat
    in
    headings ++ "\n" ++ values


{-| Transpose the rows and columns of a table. Provide the name of column that will
generate the column headings in the transposed table as the first parameter and the
name you wish to give the new row names as the second.

For example,

    newTable =
        myTable |> transposeTable "location" "temperature"

where `myTable` stores:

```markdown
| location  | temperature2017 | temperature2018 |
| --------- | --------------: | --------------: |
| Bristol   |              12 |              14 |
| Sheffield |              11 |              13 |
| Glasgow   |               8 |               9 |
```

creates the following table:

```markdown
| temperature     | Bristol | Sheffield | Glasgow |
| --------------- | ------: | --------: | ------: |
| temperature2017 |      12 |        11 |       8 |
| temperature2018 |      14 |        13 |       9 |
```

If the column to contain new headings cannot be found, an empty table is generated.
If there are repeated names in the new headings column, earlier rows are replaced
with later repeated ones.

-}
transposeTable : String -> String -> Table -> Table
transposeTable headingColumn rowName tbl =
    let
        colToList heading columns =
            heading :: (Dict.get heading columns |> Maybe.withDefault [])
    in
    case getColumn headingColumn (tableColumns tbl) of
        Just newHeadings ->
            let
                body =
                    remove headingColumn (tableColumns tbl)

                trBody =
                    (rowName :: newHeadings)
                        :: (body
                                |> Dict.keys
                                |> List.map (\( _, k ) -> k :: (getColumn k body |> Maybe.withDefault []))
                           )
                        |> transpose
            in
            List.foldl
                (\cells t ->
                    case cells of
                        hd :: tl ->
                            insertColumn hd tl t

                        _ ->
                            t
                )
                empty
                trBody

        Nothing ->
            empty



-- --------------------------------------------------------------------- Private
{- Type of data stored in the cells that make up a table. -}


type alias Cell =
    String



{- The columns that make up a table of cells. -}


type alias Columns =
    Dict Heading (List Cell)



{- Externally a column heading is just a String, but internally it is an
   (Int,String) where the first element is the column index. By incrementing
   the index after each new column is created, it preserves them in insert order
   rather than the alphabetic order we would get if just Strings were used.
-}


type alias Heading =
    ( Int, String )



{- JsVal allows every JSON type to be represented. See
   <https://stackoverflow.com/questions/40825493/elm-decoding-unknown-json-structure>
-}


type JsVal
    = JsString String
    | JsInt Int
    | JsFloat Float
    | JsArray (List JsVal)
    | JsObject (Dict String JsVal)
    | JsNull



{- Provides the first row of values for each column in the table. -}


columnsHead : Columns -> List ( Heading, Cell )
columnsHead columns =
    if (columns |> Dict.values |> List.head |> Maybe.withDefault []) == [] then
        []

    else
        Dict.foldl (\k v -> (::) ( k, List.head v |> Maybe.withDefault "" )) [] columns



{- Finds the column index number of the column with the given name, if it exists -}


columnIndex : String -> Columns -> Maybe Int
columnIndex colName =
    Dict.keys
        >> List.filter (\( _, heading ) -> heading == colName)
        >> List.head
        >> Maybe.map Tuple.first


columnsTail : Columns -> Columns
columnsTail =
    Dict.map (always (List.drop 1))



{- Re-compute column indices from 0 to (numCols-1) while preserving current order. -}


compactIndices : Int -> Columns -> Columns
compactIndices startIndex =
    Dict.foldl
        (\( _, colHeading ) v acc -> Dict.insert ( startIndex + Dict.size acc, colHeading ) v acc)
        Dict.empty


flip : (a -> b -> c) -> b -> a -> c
flip fn argB argA =
    fn argA argB



{- Like Dict.get but ignores the ordering index in the key tuple. -}


getColumn : String -> Columns -> Maybe (List String)
getColumn colName =
    Dict.foldl
        (\( _, s ) v acc ->
            case acc of
                Just _ ->
                    acc

                Nothing ->
                    if s == colName then
                        Just v

                    else
                        Nothing
        )
        Nothing



{- Private version of insertColumn that allows a column index to be defined.
   Normally new columns are inserted at the end of existing ones (by inserting at
   Dict.size) but if we need to insert before others, we can provide a negative
   index for example.
-}


insertColumnAt : Int -> String -> List String -> Columns -> Columns
insertColumnAt index heading colValues columns =
    let
        colSize =
            if columns == Dict.empty then
                List.length colValues

            else
                columns |> Dict.values |> List.head |> Maybe.withDefault [] |> List.length

        extraRows =
            List.repeat (List.length colValues - colSize) ""

        insertIndex =
            case columnIndex heading columns of
                Just i ->
                    i

                Nothing ->
                    index
    in
    columns
        |> Dict.insert ( insertIndex, String.trim heading ) (List.take colSize (colValues ++ extraRows))



{- Decodes a JSON type and stores it as one of the JsVal types. Note that this single
   type can be an array or object which can contain nested types within. See
   https://stackoverflow.com/questions/40825493/elm-decoding-unknown-json-structure
-}


jsValDecoder : Json.Decode.Decoder JsVal
jsValDecoder =
    Json.Decode.oneOf
        [ Json.Decode.map JsString Json.Decode.string
        , Json.Decode.map JsInt Json.Decode.int
        , Json.Decode.map JsFloat Json.Decode.float
        , Json.Decode.list (Json.Decode.lazy (\_ -> jsValDecoder)) |> Json.Decode.map JsArray
        , Json.Decode.dict (Json.Decode.lazy (\_ -> jsValDecoder)) |> Json.Decode.map JsObject
        , Json.Decode.null JsNull
        ]



{- Like Dict.member but ignores the ordering index in the key tuple. -}


memberColumns : String -> Columns -> Bool
memberColumns colName =
    Dict.keys >> List.map Tuple.second >> List.member colName


numRows : Table -> Int
numRows =
    tableColumns
        >> Dict.values
        >> List.head
        >> Maybe.withDefault []
        >> List.length



{- Like Dict.remove but ignores the ordering index in the key tuple. -}


remove : String -> Columns -> Columns
remove colName =
    Dict.filter (\( _, s ) _ -> s /= colName)


tableColumns : Table -> Columns
tableColumns tbl =
    case tbl of
        Table cols ->
            .columns cols


toTable : Columns -> Table
toTable cols =
    Table { columns = cols }


transpose : List (List a) -> List (List a)
transpose xss =
    let
        numCols =
            List.head >> Maybe.withDefault [] >> List.length
    in
    List.foldr (List.map2 (::)) (List.repeat (numCols xss) []) xss



{- Remove repetitions from a list preserving the original order of non-repeated
   items. From
   [List.extra](https://package.elm-lang.org/packages/elm-community/list-extra/latest/List-Extra),
-}


unique : List comparable -> List comparable
unique list =
    let
        uniqueHelp f existing remaining accumulator =
            case remaining of
                [] ->
                    List.reverse accumulator

                first :: rest ->
                    let
                        computedFirst =
                            f first
                    in
                    if Set.member computedFirst existing then
                        uniqueHelp f existing rest accumulator

                    else
                        uniqueHelp f (Set.insert computedFirst existing) rest (first :: accumulator)
    in
    uniqueHelp identity Set.empty list []



{- Zips two lists together as a list of tuples -}


zip : List a -> List b -> List ( a, b )
zip =
    List.map2 Tuple.pair
