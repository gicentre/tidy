module Tidy exposing
    ( Table
    , fromCSV
    , fromGridText
    , fromGridLists
    , empty
    , insertRow
    , filterRows
    , renameColumn
    , insertColumn
    , removeColumn
    , filterColumns
    , melt
    , transposeTable
    , leftJoin
    , rightJoin
    , innerJoin
    , outerJoin
    , leftDiff
    , rightDiff
    , tableSummary
    , numColumn
    , strColumn
    , boolColumn
    , toColumn
    )

{-| A collection of utilities for representing tabular data and reshaping them.

@docs Table


# Create

@docs fromCSV
@docs fromGridText
@docs fromGridLists
@docs empty


# Edit

@docs insertRow
@docs filterRows

@docs renameColumn
@docs insertColumn
@docs removeColumn
@docs filterColumns


# Tidy

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
@docs transposeTable


# Join

Join two tables using a common key. While not specific to tidy data, joining tidy
tables is often more meaningful than joining messy ones. The examples below illustrate
joining two input tables as follows with shared key values `k2` and `k4`:

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


## Column output

@docs numColumn
@docs strColumn
@docs boolColumn
@docs toColumn

-}

import Dict exposing (Dict)
import Regex


{-| A table of data arranged in rows and columns. Each column in a table has a
unique name by which it may be referenced. Table cell values are represented as
Strings, but can be converted to other types via column output functions
(e.g. [numColumn](#numColumn)).
-}
type
    Table
    -- Opaque type
    = Table
        { columns : Columns
        }


{-| Create an empty table. Useful if table items are to be added programatically
with `insertRow` and `insertColumn`.
-}
empty : Table
empty =
    toTable Dict.empty


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

        addEntry : ( Int, List Cell ) -> Columns -> Columns
        addEntry xs =
            case xs of
                ( n, hd :: tl ) ->
                    Dict.insert ( n, String.trim hd ) (List.map String.trim tl)

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
| --- | --- | --- |
|   0 |   0 | z00 |
|   0 |   1 | z01 |
|   0 |   2 | z02 |
|   0 |   3 | z03 |
|   1 |   0 | z10 |
|   1 |   1 | z11 |
|   : |   : |   : |
```

Note the common convention that in grids, the origin (row 0) is at the top-left
whereas in Cartesian coordinate systems the origin (y = 0) is at the bottom-left.
You may therefore wish to perform an additional transformation on the ordering of
row values in the input string if you are mapping onto a Cartesian coordinate system.

-}
fromGridText : String -> Table
fromGridText =
    String.lines
        >> List.filter (not << String.isEmpty)
        >> List.map (String.split "," >> List.filter (not << String.isEmpty))
        >> fromGridLists


{-| Transform list of input string lists in the form:

    [ [z00, z01, z02, z03, ...]
    , [z10, z11, z12, z13, ...]
    , [z20, z21, z22, c23, ...]
    , [z30, z31, z32, c33, ...]
    , [...]
    ]

into a tidy table in the form:

```markdown
| row | col |   z |
| --- | --- | --- |
|   0 |   0 | z00 |
|   0 |   1 | z01 |
|   0 |   2 | z02 |
|   0 |   3 | z03 |
|   1 |   0 | z10 |
|   1 |   1 | z11 |
|   : |   : |   : |
```

Note the common convention that in grids, the origin (row 0) is at the top-left
whereas in Cartesian coordinate systems the origin (y = 0) is at the bottom-left.
You may therefore wish to perform an additional reversing of the order of row
lists in the input if you are mapping onto a Cartesian coordinate system.

-}
fromGridLists : List (List String) -> Table
fromGridLists =
    let
        updateCol v maybeCol =
            case maybeCol of
                Nothing ->
                    Just [ v ]

                Just col ->
                    Just (v :: col)

        addGridCell ( r, c, z ) =
            Dict.update ( 0, "row" ) (updateCol r)
                >> Dict.update ( 1, "col" ) (updateCol c)
                >> Dict.update ( 2, "z" ) (updateCol z)
    in
    List.indexedMap
        (\r -> List.indexedMap (\c z -> ( String.fromInt r, String.fromInt c, String.trim z )))
        >> List.concat
        >> List.foldl addGridCell Dict.empty
        >> toTable


{-| Add a row of values to a table. The new values are represented by a list of
`(columnName,columnValue)` tuples. The columnNames should correspond to the names
of the columns in the table to which each `columnValue` is added. Names not in the
table to append are ignored and any unspecified columns have an empty string value
inserted.
-}
insertRow : List ( String, String ) -> Table -> Table
insertRow namedCells =
    -- To guarantee we don't misalign columns, we always add a complete set of
    -- column values, even if they are not all provided as parameters.
    let
        newValues =
            Dict.fromList namedCells

        newCell colHead =
            Dict.get colHead newValues |> Maybe.withDefault ""
    in
    getColumns
        >> Dict.map (\( _, colHead ) colValues -> colValues ++ [ newCell colHead ])
        >> toTable


{-| Rename the given column (first parameter) with a new name (second parameter).
If the new column name matches an exsiting one, the existing one will be replaced
by the renamed column.
-}
renameColumn : String -> String -> Table -> Table
renameColumn oldName newName =
    getColumns
        >> Dict.foldl
            (\( n, colName ) ->
                if colName == oldName then
                    Dict.insert ( n, newName )

                else
                    Dict.insert ( n, colName )
            )
            Dict.empty
        >> toTable


{-| Add a column of data to a table. The first parameter is the name to give the
column. The second is a list of column values. If the table already has a column
with this name, it will get replaced with the given data. To ensure table rows are
always aligned, if the table is not empty, the column values are padded / truncated
to match the number of rows in the table.
-}
insertColumn : String -> List String -> Table -> Table
insertColumn heading colValues tbl =
    insertColumnAt (Dict.size (getColumns tbl)) heading colValues (getColumns tbl)
        -- Need to compact indices in case the additional column replaces existing one.
        |> compactIndices 0
        |> toTable


{-| Remove a column with the given name from a table. If the column is not present
in the table, the original table is returned.
-}
removeColumn : String -> Table -> Table
removeColumn colName =
    getColumns >> remove colName >> compactIndices 0 >> toTable


{-| Extract the values of the column with the given name (first parameter) from a
table. The type of values in the column is determined by the given cell conversion
function. The converter function should handle cases of missing data in the table
as well as failed conversions (e.g. attempts to convert text into a number).

    imputeMissing : String -> Int
    imputeMissing =
        String.toFloat >> Maybe.withDefault 0

    myTable |> toColumn "count" imputeMissing

-}
toColumn : String -> (String -> a) -> Table -> List a
toColumn heading converter =
    getColumns
        >> getColumn heading
        >> Maybe.withDefault []
        >> List.map converter


{-| Extract the numeric values of a given column from a table. Any conversions
that fail, including missing values in the table are converted into zeros. If
you wish to handle missing data / failed conversions in a different way, use
[toColumn](#toColumn) instead, providing a custom converter function.

    myTable |> numColumn "year"

-}
numColumn : String -> Table -> List Float
numColumn heading =
    toColumn heading (String.toFloat >> Maybe.withDefault 0)


{-| Extract the string values of a given column from a table. Missing values in
the table are represented as empty strings. If you wish to handle missing values
in a different way, use [toColumn](#toColumn) instead, providing a custom converter
function.

    myTable |> strColumn "cityName"

-}
strColumn : String -> Table -> List String
strColumn heading =
    toColumn heading identity


{-| Extract Boolean values of a given column from a table. Assumes that `True`
values can be represented by the case-insenstive strings `true`, `yes` and `1`
while all other values are assumed to be false.

      myTable |> toBool "isMarried"

-}
boolColumn : String -> Table -> List Bool
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
            --tbl |> getColumns |> Dict.keys |> List.map (\( n, s ) -> String.fromInt n ++ "," ++ s) |> addDividers
            tbl |> getColumns |> Dict.keys |> List.map Tuple.second |> addDividers

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


{-| Transpose the rows and columns of a table. To do this you need to provide the
name of column that will generate the column headings in the transposed table
(first parameter) and the name you wish to give the new row names (second parameter).

For example,

    transposeTable "location" "temperature" table

where `table` stores:

```markdown
| location  | temperature2017 | temperature2018 |
| --------- | --------------- | --------------- |
| Bristol   | 12              | 14              |
| Sheffield | 11              | 13              |
| Glasgow   |  8              |  9              |
```

creates the following table:

```markdown
| temperature     | Bristol | Sheffield | Glasgow |
| --------------- | ------- | --------- | ------- |
| temperature2017 | 12      | 11        | 8       |
| temperature2018 | 14      | 13        | 9       |
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
    case getColumn headingColumn (getColumns tbl) of
        Just newHeadings ->
            let
                body =
                    remove headingColumn (getColumns tbl)

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

    """location,temperature2017,temperature2018
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
        colToVarLookup : Dict String Cell
        colToVarLookup =
            Dict.fromList colVars

        numCols =
            Dict.size (getColumns table)

        emptyColumns : Columns
        emptyColumns =
            table
                |> getColumns
                |> Dict.keys
                |> List.filter (\( _, s ) -> not (List.member s (List.map Tuple.first colVars)))
                |> (++) [ ( numCols, columnName ), ( numCols + 1, valueName ) ]
                |> List.map (\label -> ( label, [] ))
                |> Dict.fromList

        newRows : Columns -> List (List ( Heading, Cell ))
        newRows columns =
            let
                meltCol ( ( _, oldHeading ), val ) =
                    case Dict.get oldHeading colToVarLookup of
                        Just colRef ->
                            Just [ ( ( numCols, columnName ), colRef ), ( ( numCols + 1, valueName ), val ) ]

                        Nothing ->
                            Nothing

                unmeltedCol ( ( n, oldHeading ), val ) =
                    case Dict.get oldHeading colToVarLookup of
                        Just colRef ->
                            Nothing

                        Nothing ->
                            Just ( ( n, oldHeading ), val )

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
    extractRows ( getColumns table, emptyColumns ) |> Tuple.second |> compactIndices 0 |> toTable


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
            case getColumn columnName (getColumns tbl) of
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
filterColumns : (String -> Bool) -> Table -> Table
filterColumns fn =
    getColumns
        >> Dict.filter (\( _, colHeading ) _ -> fn colHeading)
        >> compactIndices 0
        >> toTable


{-| A _left join_ preserves all the values in the first table and adds any key-matched
values from columns in the second table to it. Where both tables share common column
names, including key columns, only those in the left (first) table are stored in the output.

    leftJoin ( table1, "Key1" ) ( table2, "Key2" )

would generate

```markdown
| Key1 | colA | colB | Key2 | colC | colD |
| ---- | ---- | ---- | --- | ---- | ---- |
| k1   | a1   | b1   |     |      |      |
| k2   | a2   | b2   | k2  | c2   | d2   |
| k3   | a3   | b3   |     |      |      |
| k4   | a4   | b4   | k4  | c4   | d4   |
```

If one or both of the key columns are not found, the left table is returned.

-}
leftJoin : ( Table, String ) -> ( Table, String ) -> Table
leftJoin ( t1, key1 ) ( t2, key2 ) =
    -- Check that the named keys are present in both tables
    if not <| memberColumns key1 (getColumns t1) && memberColumns key2 (getColumns t2) then
        t1

    else
        let
            keyCol colLabel =
                case ( getColumn key2 (getColumns t2), getColumn colLabel (getColumns t2) ) of
                    ( Just ks, Just vs ) ->
                        Dict.fromList (List.map2 Tuple.pair ks vs)

                    _ ->
                        Dict.empty

            tableCol colLabel =
                List.map (\colHead -> Dict.get colHead (keyCol colLabel) |> Maybe.withDefault "")
                    (getColumn key1 (getColumns t1) |> Maybe.withDefault [])

            leftInsert ( n, label2 ) columns =
                if memberColumns label2 columns then
                    columns

                else
                    columns |> Dict.insert ( n, label2 ) (tableCol label2)
        in
        List.foldl leftInsert (getColumns t1) (Dict.keys (getColumns t2 |> compactIndices (Dict.size (getColumns t1))))
            |> compactIndices 0
            |> toTable


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


{-| An 'inner join' will contain only key-matched rows that are present in both tables.
The first parameter is the name to give the new key-matched column, replacing the
separate key names in the two tables. Where both tables share a common column name,
only one is stored in the output.

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
    if not <| memberColumns key1 (getColumns oldT1) && memberColumns key2 (getColumns oldT2) then
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
                getColumn keyName (getColumns t2) |> Maybe.withDefault []
        in
        leftJoin ( t1, keyName ) ( t2, keyName )
            |> filterRows keyName (\s -> List.member s t2Keys)


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
    -- Check that the named keys are present in both tables
    if not <| memberColumns key1 (getColumns oldT1) && memberColumns key2 (getColumns oldT2) then
        empty

    else
        let
            t1 =
                oldT1 |> renameColumn key1 keyName

            t2 =
                oldT2 |> renameColumn key2 keyName

            leftColumns =
                leftJoin ( t1, keyName ) ( t2, keyName ) |> getColumns

            rightTable =
                rightJoin ( t1, keyName ) ( t2, keyName )

            diff =
                filterRows keyName
                    (\s -> not <| List.member s (getColumn keyName leftColumns |> Maybe.withDefault []))
                    rightTable
        in
        Dict.map (\( n, k ) v -> v ++ (getColumn k (getColumns diff) |> Maybe.withDefault [])) leftColumns
            |> toTable


{-| Provides table of all the rows in the first table that do not occur in any key-matched
rows in the second table.

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
    if not <| memberColumns key1 (getColumns t1) then
        empty

    else
        let
            t2Keys =
                getColumn key2 (getColumns t2) |> Maybe.withDefault []
        in
        filterRows key1 (\s -> not <| List.member s t2Keys) t1


{-| Provides table of all the rows in the second table that do not occur in any key-matched
rows in the first table.

    rightDiff ( table1, "Key1" ) ( table2, "Key2" )

would generate

```markdown
| Key2 | colA | colB |
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



-- ------------------------------- Private
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



{- Provides the first row of values for each column in the table. -}


columnsHead : Columns -> List ( Heading, Cell )
columnsHead columns =
    if List.foldl (max << List.length) 0 (Dict.values columns) == 0 then
        []

    else
        Dict.foldl (\k v -> (::) ( k, List.head v |> Maybe.withDefault "" )) [] columns



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



{- Like Dict.member but ignores the ordering index in the key tuple. -}


memberColumns : String -> Columns -> Bool
memberColumns colName =
    Dict.keys >> List.map Tuple.second >> List.member colName



{- Like Dict.remove but ignores the ordering index in the key tuple. -}


remove : String -> Columns -> Columns
remove colName =
    Dict.filter (\( _, s ) _ -> s /= colName)


{-| Private version of insertColumn that allows a column index to be defined.
Normally new columns are inserted at the end of existing ones (by inserting at
Dict.size) but if we need to insert before others, we can provide a negative
index for example.
-}
insertColumnAt : Int -> String -> List String -> Columns -> Columns
insertColumnAt index heading colValues columns =
    let
        numRows =
            if columns == Dict.empty then
                List.length colValues

            else
                List.foldl (max << List.length) 0 (Dict.values columns)

        extraRows =
            List.repeat (List.length colValues - numRows) ""
    in
    columns
        |> Dict.insert ( index, String.trim heading ) (List.take numRows (colValues ++ extraRows))



{- Re-compute column indices from 0 to (numCols-1) while preserving current order. -}


compactIndices : Int -> Columns -> Columns
compactIndices startIndex =
    Dict.foldl
        (\( _, colHeading ) v acc -> Dict.insert ( startIndex + Dict.size acc, colHeading ) v acc)
        Dict.empty


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
