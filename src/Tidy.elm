module Tidy exposing
    ( Table
    , Cell
    , tableSummary
    , fromCSV
    , fromGridText
    , fromGridLists
    , addRow
    , addColumn
    , empty
    , melt
    , transposeTable
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
@docs Cell
@docs tableSummary


## Table Generation

@docs fromCSV
@docs fromGridText
@docs fromGridLists
@docs addRow
@docs addColumn
@docs empty


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
@docs transposeTable


## Table Joining

Join two tables using a common key. While not specific to tidy data, joining tidy
tables is often more meaningful than joining messy ones. The examples below illustrate
joining two input tables as follows with shared key values k2 and k4:

```markdown
table1:

| Key1 | colLabelA | colLabelB |
| ---- | --------- | --------- |
| k1   | a1        | b1        |
| k2   | a2        | b2        |
| k3   | a3        | b3        |
| k4   | a4        | b4        |

table2:

| Key2 | colLabelC | colLabelD |
| ---- | --------- | --------- |
| k2   | c2        | d2        |
| k4   | c4        | d4        |
| k6   | c6        | d6        |
| k8   | c8        | d8        |
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


{-| Type of data stored in the cells that make up a table.
-}
type alias Cell =
    String


{-| Create an empty table. Useful if table items are to be added programatically
with `addRow` and `addColumn`.
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
|   0 |   1 | z00 |
|   0 |   2 | z00 |
|   0 |   3 | z00 |
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


{-| Transform list of lists input strings in the form:

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
|   0 |   1 | z00 |
|   0 |   2 | z00 |
|   0 |   3 | z00 |
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
addRow : List ( String, String ) -> Table -> Table
addRow namedCells =
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


{-| Add a column of data to a table. The first parameter is the name to give the
column. The second is a list of column values. If the table already has a column
with this name, it will get replaced with the given data. To ensure table rows are
always aligned, if the table is not empty, the column values are padded / truncated
to match the number of rows in the table.
-}
addColumn : String -> List String -> Table -> Table
addColumn heading colValues tbl =
    let
        numRows =
            if getColumns tbl == Dict.empty then
                List.length colValues

            else
                List.foldl (max << List.length) 0 (Dict.values (getColumns tbl))

        extraRows =
            List.repeat (List.length colValues - numRows) ""

        numCols =
            Dict.size (getColumns tbl)
    in
    getColumns tbl
        |> Dict.insert ( numCols, String.trim heading ) (List.take numRows (colValues ++ extraRows))
        |> toTable


{-| Extract the values of the column with the given name (first parameter) from a
table. The type of values in the column is determined by the given cell conversion
function. The converter function should handle cases of missing data in the table
as well as failed conversions (e.g. attempts to convert text into a number).

    imputeMissing : Cell -> Int
    imputeMissing =
        String.toFloat >> Maybe.withDefault 0

    myTable |> toColumn "count" imputeMissing

-}
toColumn : String -> (Cell -> a) -> Table -> List a
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
| temperature     | Bristol | Glasgow | Sheffield
| --------------- | ------- | ------- | --------- |
| temperature2017 | 12      | 8       | 11        |
| temperature2018 | 14      | 9       | 13        |
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
                    removeColumn headingColumn (getColumns tbl)

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
                            addColumn hd tl t

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
melt : String -> String -> List ( String, Cell ) -> Table -> Table
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
    -- TODO: Should we pass through an index compactor to  ensure column indices go from 0 to (numCols-1)?
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
filterRows : String -> (Cell -> Bool) -> Table -> Table
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
    getColumns >> Dict.filter (\( _, colHeading ) _ -> fn colHeading) >> toTable


{-| A _left join_ preserves all the values in the first table and adds any key-matched
values from columns in the second table to it. Where both tables share common column
names, only those in the left (first) table are stored in the output.

    leftJoin ( table1, "Key1" ) ( table2, "Key2" )

would generate

```markdown
| Key1 | colA | colB | colC | colD |
| ---- | ---- | ---- | ---- | ---- |
| k1   | a1   | b1   |      |      |
| k2   | a2   | b2   | c2   | d2   |
| k3   | a3   | b3   |      |      |
| k4   | a4   | b4   | c4   | d4   |
```

-}
leftJoin : ( Table, String ) -> ( Table, String ) -> Table
leftJoin ( t1, heading1 ) ( t2, heading2 ) =
    -- TODO: Account for different key names in the two tables. Include descriptions in comments for what happens if both keys share a common name and if they do not.
    let
        keyCol colLabel =
            case ( getColumn heading2 (getColumns t2), Dict.get colLabel (getColumns t2) ) of
                ( Just ks, Just vs ) ->
                    Dict.fromList (List.map2 Tuple.pair ks vs)

                _ ->
                    Dict.empty

        tableCol colLabel =
            List.map (\k -> Dict.get k (keyCol colLabel) |> Maybe.withDefault "")
                (getColumn heading1 (getColumns t1) |> Maybe.withDefault [])

        leftInsert label2 columns =
            if Dict.member label2 columns then
                columns

            else
                columns |> Dict.insert label2 (tableCol label2)

        -- TODO: Need to reallocate column indices to preserve column orders (left table then right table) by adding size of t1 to all column indices of t2
    in
    toTable (List.foldl leftInsert (getColumns t1) (Dict.keys (getColumns t2)))


{-| A _right join_ preserves all the values in the second table and adds any key-matched
values from columns in the first table to it. Where both tables share common column
names, only those in the right (second) table are stored in the output.

    rightJoin ( table1, "Key1" ) ( table2, "Key2" )

would generate

```markdown
| Key2 | colA | colB | colC | colD |
| ---- | ---- | ---- | ---- | ---- |
| k2   | a2   | b2   | c2   | d2   |
| k4   | a4   | b4   | c4   | d4   |
| k6   |      |      | c6   | d6   |
| k8   |      |      | c8   | d8   |
```

-}
rightJoin : ( Table, String ) -> ( Table, String ) -> Table
rightJoin =
    -- TODO: Account for different key names in the two tables. Include descriptions in comments for what happens if both keys share a common name and if they do not.
    flip leftJoin


{-| An 'inner join' will contain only key-matched rows that are present in both tables.
The first parameter is the name to give the new key-matched column.

    innerJoin "Key" ( table1, "Key1" ) ( table2, "Key2" )

would generate

```markdown
| Key | colA | colB | colC | colD |
| --- | ---- | ---- | ---- | ---- |
| k2  | a2   | b2   | c2   | d2   |
| k4  | a4   | b4   | c4   | d4   |
```

-}
innerJoin : String -> ( Table, String ) -> ( Table, String ) -> Table
innerJoin keyName ( t1, heading1 ) ( t2, heading2 ) =
    -- TODO: Set key to keyname
    -- TODO: Account for different key names in the two tables. Include descriptions in comments for what happens if both keys share a common name and if they do not.
    let
        keyCol colLabel =
            case ( getColumn heading2 (getColumns t2), Dict.get colLabel (getColumns t2) ) of
                ( Just ks, Just vs ) ->
                    Dict.fromList (List.map2 Tuple.pair ks vs)

                _ ->
                    Dict.empty

        tableCol colLabel =
            List.map (\k -> Dict.get k (keyCol colLabel) |> Maybe.withDefault "")
                (getColumn heading1 (getColumns t1) |> Maybe.withDefault [])
    in
    List.foldl (\label2 -> Dict.insert label2 (tableCol label2)) (getColumns t1) (Dict.keys (getColumns t2))
        |> toTable
        |> filterRows heading2 (not << String.isEmpty)


{-| An _outer join_ contains all rows of both joined tables.

    outerJoin ( table1, "Key1" ) ( table2, "Key2" )

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

-}
outerJoin : ( Table, String ) -> ( Table, String ) -> Table
outerJoin ( t1, heading1 ) ( t2, heading2 ) =
    -- TODO: Account for different key names in the two tables. Include descriptions in comments for what happens if both keys share a common name and if they do not.
    let
        leftColumns =
            leftJoin ( t1, heading1 ) ( t2, heading2 ) |> getColumns

        rightTable =
            rightJoin ( t1, heading1 ) ( t2, heading2 )

        diff =
            filterRows heading2
                (\s -> Basics.not (List.member s (getColumn heading1 leftColumns |> Maybe.withDefault [])))
                rightTable
    in
    Dict.map (\k v -> v ++ (Dict.get k (getColumns diff) |> Maybe.withDefault [])) leftColumns
        |> toTable



-- ------------------------------- Private
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
getColumn colName columns =
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
        columns



{- Like Dict.remove but ignores the ordering index in the key tuple. -}


removeColumn : String -> Columns -> Columns
removeColumn colName =
    Dict.filter (\( _, s ) _ -> s /= colName)


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
