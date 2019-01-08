module TidyTests exposing (suite)

import Expect exposing (Expectation)
import Test exposing (..)
import Tidy


suite : Test
suite =
    let
        table1 =
            """Treatment,John Smith,Jane Doe,Mary Johnson
treatmenta,  , 16, 3
treatmentb, 2, 11, 1"""
                |> Tidy.fromCSV

        table2 =
            Tidy.empty
                |> Tidy.addColumn "Treatment" [ "treatmenta", "treatmentb" ]
                |> Tidy.addColumn "John Smith" [ "", "2" ]
                |> Tidy.addColumn "Jane Doe" [ "16", "11" ]
                |> Tidy.addColumn "Mary Johnson" [ "3", "1" ]

        table3 =
            Tidy.empty
                |> Tidy.addColumn "Treatment" []
                |> Tidy.addColumn "John Smith" []
                |> Tidy.addColumn "Jane Doe" []
                |> Tidy.addColumn "Mary Johnson" []
                |> Tidy.addRow [ ( "Treatment", "treatmenta" ), ( "John Smith", "" ), ( "Jane Doe", "16" ), ( "Mary Johnson", "3" ) ]
                |> Tidy.addRow [ ( "Treatment", "treatmentb" ), ( "John Smith", "2" ), ( "Jane Doe", "11" ), ( "Mary Johnson", "1" ) ]

        table4 =
            Tidy.empty
                |> Tidy.addColumn "Person" [ "John Smith", "Jane Doe", "Mary Johnson" ]
                |> Tidy.addColumn "treatmenta" [ "", "16", "3" ]
                |> Tidy.addColumn "treatmentb" [ "2", "11", "1" ]
    in
    describe "Table generation and column output conversion"
        [ describe "fromCSV"
            [ test "CSV treatment column" <|
                \_ ->
                    Tidy.strColumn "Treatment" table1 |> Expect.equal [ "treatmenta", "treatmentb" ]
            , test "CSV column with all items" <|
                \_ ->
                    Tidy.strColumn "Jane Doe" table1 |> Expect.equal [ "16", "11" ]
            , test "CSV column with missing items" <|
                \_ ->
                    Tidy.strColumn "John Smith" table1 |> Expect.equal [ "", "2" ]
            , test "CSV numeric output of column with all items" <|
                \_ ->
                    Tidy.numColumn "Jane Doe" table1 |> Expect.equal [ 16, 11 ]
            , test "CSV numeric column with missing items" <|
                \_ ->
                    Tidy.numColumn "John Smith" table1 |> Expect.equal [ 0, 2 ]
            , test "CSV numeric conversion of non-numeric column" <|
                \_ ->
                    Tidy.numColumn "Treatment" table1 |> Expect.equal [ 0, 0 ]
            ]
        , describe "programaticTables"
            [ test "addingColumns" <|
                \_ ->
                    table1 |> Expect.equal table2
            , test "addingRows" <|
                \_ ->
                    table1 |> Expect.equal table3
            , test "transposing" <|
                \_ ->
                    table4 |> Expect.equal (Tidy.transposeTable "Treatment" "Person" table1)
            ]
        ]
