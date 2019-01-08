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
                |> Tidy.insertColumn "Treatment" [ "treatmenta", "treatmentb" ]
                |> Tidy.insertColumn "John Smith" [ "", "2" ]
                |> Tidy.insertColumn "Jane Doe" [ "16", "11" ]
                |> Tidy.insertColumn "Mary Johnson" [ "3", "1" ]

        table3 =
            Tidy.empty
                |> Tidy.insertColumn "Treatment" []
                |> Tidy.insertColumn "John Smith" []
                |> Tidy.insertColumn "Jane Doe" []
                |> Tidy.insertColumn "Mary Johnson" []
                |> Tidy.insertRow [ ( "Treatment", "treatmenta" ), ( "John Smith", "" ), ( "Jane Doe", "16" ), ( "Mary Johnson", "3" ) ]
                |> Tidy.insertRow [ ( "Treatment", "treatmentb" ), ( "John Smith", "2" ), ( "Jane Doe", "11" ), ( "Mary Johnson", "1" ) ]

        table4 =
            Tidy.empty
                |> Tidy.insertColumn "Person" [ "John Smith", "Jane Doe", "Mary Johnson" ]
                |> Tidy.insertColumn "treatmenta" [ "", "16", "3" ]
                |> Tidy.insertColumn "treatmentb" [ "2", "11", "1" ]

        jTable1 =
            Tidy.empty
                |> Tidy.insertColumn "Key1" [ "k1", "k2", "k3", "k4" ]
                |> Tidy.insertColumn "colA" [ "a1", "a2", "a3", "a4" ]
                |> Tidy.insertColumn "colB" [ "b1", "b2", "b3", "b4" ]

        jTable2 =
            Tidy.empty
                |> Tidy.insertColumn "Key2" [ "k2", "k4", "k6", "k8" ]
                |> Tidy.insertColumn "colC" [ "c2", "c4", "c6", "c8" ]
                |> Tidy.insertColumn "colD" [ "d2", "d4", "d6", "d8" ]

        ljTable =
            Tidy.empty
                |> Tidy.insertColumn "Key1" [ "k1", "k2", "k3", "k4" ]
                |> Tidy.insertColumn "colA" [ "a1", "a2", "a3", "a4" ]
                |> Tidy.insertColumn "colB" [ "b1", "b2", "b3", "b4" ]
                |> Tidy.insertColumn "Key2" [ "", "k2", "", "k4" ]
                |> Tidy.insertColumn "colC" [ "", "c2", "", "c4" ]
                |> Tidy.insertColumn "colD" [ "", "d2", "", "d4" ]

        rjTable =
            Tidy.empty
                |> Tidy.insertColumn "Key2" [ "k2", "k4", "k6", "k8" ]
                |> Tidy.insertColumn "colC" [ "c2", "c4", "c6", "c8" ]
                |> Tidy.insertColumn "colD" [ "d2", "d4", "d6", "d8" ]
                |> Tidy.insertColumn "Key1" [ "k2", "k4", "", "" ]
                |> Tidy.insertColumn "colA" [ "a2", "a4", "", "" ]
                |> Tidy.insertColumn "colB" [ "b2", "b4", "", "" ]

        ijTable =
            Tidy.empty

        ojTable =
            Tidy.empty
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
        , describe "joining"
            [ test "leftJoin" <|
                \_ ->
                    Tidy.leftJoin ( jTable1, "Key1" ) ( jTable2, "Key2" ) |> Expect.equal ljTable
            , test "rightJoin" <|
                \_ ->
                    Tidy.rightJoin ( jTable1, "Key1" ) ( jTable2, "Key2" ) |> Expect.equal rjTable
            ]
        ]
