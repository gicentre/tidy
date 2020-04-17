module TidyTests exposing (suite)

import Expect exposing (Expectation)
import Test exposing (..)
import Tidy


suite : Test
suite =
    let
        table1 =
            Tidy.empty
                |> Tidy.insertColumn "Treatment" [ "treatmenta", "treatmentb" ]
                |> Tidy.insertColumn "John Smith" [ "", "2" ]
                |> Tidy.insertColumn "Jane Doe" [ "16", "11" ]
                |> Tidy.insertColumn "Mary Johnson" [ "3", "1" ]

        table1CSV =
            """Treatment,John Smith,Jane Doe,Mary Johnson
treatmenta,, 16, 3
treatmentb, 2, 11, 1"""
                |> Tidy.fromCSV

        json =
            """[
          { "person": "John Smith", "treatment": "b", "result": 2 },
          { "person": "Jane Doe", "treatment": "a", "result": 16 },
          { "person": "Jane Doe", "treatment": "b", "result": 11 },
          { "person": "Mary Johnson", "treatment": "a", "result": 3 },
          { "person": "Mary Johnson", "treatment": "b", "result": 1 }
        ]"""

        tidyTreatmentJson =
            Tidy.empty
                |> Tidy.insertColumnFromJson "person" [] json
                |> Tidy.insertColumnFromJson "treatment" [] json
                |> Tidy.insertColumnFromJson "result" [] json

        tidyTreatment =
            Tidy.empty
                |> Tidy.insertColumn "person" [ "John Smith", "Jane Doe", "Jane Doe", "Mary Johnson", "Mary Johnson" ]
                |> Tidy.insertColumn "treatment" [ "b", "a", "b", "a", "b" ]
                |> Tidy.insertColumn "result" [ "2", "16", "11", "3", "1" ]

        -- Note: We need to use explict '\t's to represent tabs to avoid editor reformatting as spaces.
        table1TSV =
            """Treatment\tJohn Smith\tJane Doe\tMary Johnson
treatmenta\t\t16\t3
treatmentb\t2\t11\t1"""
                |> Tidy.fromDelimited '\t'

        complexCSV =
            """Name,Value
            James,1
"James Bond",2




"Bond, James Bond",007
Miss Moneypenny,3,,,,,,
Spectre      ,    4   """

        complexTSV =
            """Name\tValue
            James\t1
"James Bond"\t2




"Bond, James Bond"\t007
Miss Moneypenny\t3
Spectre    \t       4   """

        complexTable =
            Tidy.empty
                |> Tidy.insertColumn "Name" [ "James", "James Bond", "Bond, James Bond", "Miss Moneypenny", "Spectre" ]
                |> Tidy.insertColumn "Value" [ "1", "2", "007", "3", "4" ]

        gridTextTable =
            """z00,z01,z02,z03
z10,z11,z12,z13
z20,z21,z22,z23""" |> Tidy.fromGrid

        gridTextRaggedTable =
            """  z00,z01,z02,z03

z10,z11,z12,z13,extra

z20,    z21, z22, z23
   ,
"""
                |> Tidy.fromGrid

        gridRowsTable =
            [ [ "z00", "z01", "z02", "z03" ]
            , [ "z10", "z11", "z12", "z13" ]
            , [ "z20", "z21", "z22", "z23" ]
            ]
                |> Tidy.fromGridRows

        gridRowsRaggedTable =
            [ [ "z00", "z01", "z02", "z03" ]
            , []
            , [ "z10", "z11", "z12", "z13", "extra" ]
            , [ "z20", "z21", "z22", "z23" ]
            , [ "", "" ]
            ]
                |> Tidy.fromGridRows

        gridTable =
            Tidy.empty
                |> Tidy.insertColumn "row" [ "0", "0", "0", "0", "1", "1", "1", "1", "2", "2", "2", "2" ]
                |> Tidy.insertColumn "col" [ "0", "1", "2", "3", "0", "1", "2", "3", "0", "1", "2", "3" ]
                |> Tidy.insertColumn "z" [ "z00", "z01", "z02", "z03", "z10", "z11", "z12", "z13", "z20", "z21", "z22", "z23" ]

        gridTableBisected =
            Tidy.empty
                |> Tidy.insertColumn "row" [ "0", "0", "0", "0", "1", "1", "1", "1", "2", "2", "2", "2" ]
                |> Tidy.insertColumn "col" [ "0", "1", "2", "3", "0", "1", "2", "3", "0", "1", "2", "3" ]
                |> Tidy.insertColumn "zr" [ "z0", "z0", "z0", "z0", "z1", "z1", "z1", "z1", "z2", "z2", "z2", "z2" ]
                |> Tidy.insertColumn "zc" [ "z0", "z1", "z2", "z3", "z0", "z1", "z2", "z3", "z0", "z1", "z2", "z3" ]

        gridTableRagged =
            Tidy.empty
                |> Tidy.insertColumn "row" [ "0", "0", "0", "0", "1", "1", "1", "1", "1", "2", "2", "2", "2", "3", "3" ]
                |> Tidy.insertColumn "col" [ "0", "1", "2", "3", "0", "1", "2", "3", "4", "0", "1", "2", "3", "0", "1" ]
                |> Tidy.insertColumn "z" [ "z00", "z01", "z02", "z03", "z10", "z11", "z12", "z13", "extra", "z20", "z21", "z22", "z23", "", "" ]

        raggedCSV =
            """a,b,c,d
        1,2,3,4
        1,2,3
        1,2
        1
        1,,
        1,,,
        1,,,,
        1,,,,,,,,,,,,"""

        raggedTable =
            Tidy.empty
                |> Tidy.insertColumn "a" (List.repeat 8 "1")
                |> Tidy.insertColumn "b" (List.repeat 3 "2")
                |> Tidy.insertColumn "c" (List.repeat 2 "3")
                |> Tidy.insertColumn "d" (List.repeat 1 "4")

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

        table4NoBlanks =
            Tidy.empty
                |> Tidy.insertColumn "Person" [ "John Smith", "Jane Doe", "Mary Johnson" ]
                |> Tidy.insertColumn "treatmenta" [ "0", "16", "3" ]
                |> Tidy.insertColumn "treatmentb" [ "2", "11", "1" ]

        jTable1DiffKey =
            Tidy.empty
                |> Tidy.insertColumn "Key1" [ "k1", "k2", "k3", "k4" ]
                |> Tidy.insertColumn "colA" [ "a1", "a2", "a3", "a4" ]
                |> Tidy.insertColumn "colB" [ "b1", "b2", "b3", "b4" ]

        jTable1SameKey =
            Tidy.empty
                |> Tidy.insertColumn "Key" [ "k1", "k2", "k3", "k4" ]
                |> Tidy.insertColumn "colA" [ "a1", "a2", "a3", "a4" ]
                |> Tidy.insertColumn "colB" [ "b1", "b2", "b3", "b4" ]

        jTable2DiffKey =
            Tidy.empty
                |> Tidy.insertColumn "Key2" [ "k2", "k4", "k6", "k8" ]
                |> Tidy.insertColumn "colC" [ "c2", "c4", "c6", "c8" ]
                |> Tidy.insertColumn "colD" [ "d2", "d4", "d6", "d8" ]

        jTable2SameKey =
            Tidy.empty
                |> Tidy.insertColumn "Key" [ "k2", "k4", "k6", "k8" ]
                |> Tidy.insertColumn "colC" [ "c2", "c4", "c6", "c8" ]
                |> Tidy.insertColumn "colD" [ "d2", "d4", "d6", "d8" ]

        ljTableDiffKey =
            Tidy.empty
                |> Tidy.insertColumn "Key1" [ "k1", "k2", "k3", "k4" ]
                |> Tidy.insertColumn "colA" [ "a1", "a2", "a3", "a4" ]
                |> Tidy.insertColumn "colB" [ "b1", "b2", "b3", "b4" ]
                |> Tidy.insertColumn "Key2" [ "", "k2", "", "k4" ]
                |> Tidy.insertColumn "colC" [ "", "c2", "", "c4" ]
                |> Tidy.insertColumn "colD" [ "", "d2", "", "d4" ]

        ljTableSameKey =
            Tidy.empty
                |> Tidy.insertColumn "Key" [ "k1", "k2", "k3", "k4" ]
                |> Tidy.insertColumn "colA" [ "a1", "a2", "a3", "a4" ]
                |> Tidy.insertColumn "colB" [ "b1", "b2", "b3", "b4" ]
                |> Tidy.insertColumn "colC" [ "", "c2", "", "c4" ]
                |> Tidy.insertColumn "colD" [ "", "d2", "", "d4" ]

        ldTableDiffKey =
            Tidy.empty
                |> Tidy.insertColumn "Key1" [ "k1", "k3" ]
                |> Tidy.insertColumn "colA" [ "a1", "a3" ]
                |> Tidy.insertColumn "colB" [ "b1", "b3" ]

        ldTableSameKey =
            Tidy.empty
                |> Tidy.insertColumn "Key" [ "k1", "k3" ]
                |> Tidy.insertColumn "colA" [ "a1", "a3" ]
                |> Tidy.insertColumn "colB" [ "b1", "b3" ]

        rjTableDiffKey =
            Tidy.empty
                |> Tidy.insertColumn "Key2" [ "k2", "k4", "k6", "k8" ]
                |> Tidy.insertColumn "colC" [ "c2", "c4", "c6", "c8" ]
                |> Tidy.insertColumn "colD" [ "d2", "d4", "d6", "d8" ]
                |> Tidy.insertColumn "Key1" [ "k2", "k4", "", "" ]
                |> Tidy.insertColumn "colA" [ "a2", "a4", "", "" ]
                |> Tidy.insertColumn "colB" [ "b2", "b4", "", "" ]

        rjTableSameKey =
            Tidy.empty
                |> Tidy.insertColumn "Key" [ "k2", "k4", "k6", "k8" ]
                |> Tidy.insertColumn "colC" [ "c2", "c4", "c6", "c8" ]
                |> Tidy.insertColumn "colD" [ "d2", "d4", "d6", "d8" ]
                |> Tidy.insertColumn "colA" [ "a2", "a4", "", "" ]
                |> Tidy.insertColumn "colB" [ "b2", "b4", "", "" ]

        rdTableDiffKey =
            Tidy.empty
                |> Tidy.insertColumn "Key2" [ "k6", "k8" ]
                |> Tidy.insertColumn "colC" [ "c6", "c8" ]
                |> Tidy.insertColumn "colD" [ "d6", "d8" ]

        rdTableSameKey =
            Tidy.empty
                |> Tidy.insertColumn "Key" [ "k6", "k8" ]
                |> Tidy.insertColumn "colC" [ "c6", "c8" ]
                |> Tidy.insertColumn "colD" [ "d6", "d8" ]

        ijTable =
            Tidy.empty
                |> Tidy.insertColumn "NewKey" [ "k2", "k4" ]
                |> Tidy.insertColumn "colA" [ "a2", "a4" ]
                |> Tidy.insertColumn "colB" [ "b2", "b4" ]
                |> Tidy.insertColumn "colC" [ "c2", "c4" ]
                |> Tidy.insertColumn "colD" [ "d2", "d4" ]

        ojTable =
            Tidy.empty
                |> Tidy.insertColumn "NewKey" [ "k1", "k2", "k3", "k4", "k6", "k8" ]
                |> Tidy.insertColumn "colA" [ "a1", "a2", "a3", "a4", "", "" ]
                |> Tidy.insertColumn "colB" [ "b1", "b2", "b3", "b4", "", "" ]
                |> Tidy.insertColumn "colC" [ "", "c2", "", "c4", "c6", "c8" ]
                |> Tidy.insertColumn "colD" [ "", "d2", "", "d4", "d6", "d8" ]

        gatherTable =
            Tidy.empty
                |> Tidy.insertColumn "location" [ "Bristol", "Bristol", "Sheffield", "Sheffield", "Glasgow", "Glasgow", "Aberdeen" ]
                |> Tidy.insertColumn "year" (List.repeat 7 "2018")
                |> Tidy.insertColumn "readingType" [ "minTemp", "maxTemp", "minTemp", "maxTemp", "minTemp", "maxTemp", "maxTemp" ]
                |> Tidy.insertColumn "temperature" [ "3", "27", "-2", "26", "-10", "23", "14" ]

        spreadTable =
            Tidy.empty
                |> Tidy.insertColumn "location" [ "Bristol", "Sheffield", "Glasgow", "Aberdeen" ]
                |> Tidy.insertColumn "year" (List.repeat 4 "2018")
                |> Tidy.insertColumn "minTemp" [ "3", "-2", "-10", "" ]
                |> Tidy.insertColumn "maxTemp" [ "27", "26", "23", "14" ]

        redundantTable =
            Tidy.empty
                |> Tidy.insertColumn "owner" [ "Simone", "Marj", "Arthur", "Craig" ]
                |> Tidy.insertColumn "animal" [ "cat", "cat", "fish", "dog" ]
                |> Tidy.insertColumn "name" [ "Tiddles", "Tiddles", "Gulpie", "Rover" ]
                |> Tidy.insertColumn "age" [ "6", "6", "2", "12" ]

        keyTable =
            Tidy.empty
                |> Tidy.insertColumn "id" [ "1", "2", "3" ]
                |> Tidy.insertColumn "animal" [ "cat", "dog", "fish" ]
                |> Tidy.insertColumn "name" [ "Tiddles", "Rover", "Gulpie" ]
                |> Tidy.insertColumn "age" [ "6", "12", "2" ]

        valueTable =
            Tidy.empty
                |> Tidy.insertColumn "owner" [ "Simone", "Marj", "Arthur", "Craig" ]
                |> Tidy.insertColumn "id" [ "1", "1", "3", "2" ]
    in
    describe "Table generation and column output conversion"
        [ describe "fromCSV"
            [ test "CSV simple table" <|
                \_ ->
                    table1CSV |> Expect.equal table1
            , test "CSV treatment column" <|
                \_ ->
                    Tidy.strColumn "Treatment" table1CSV |> Expect.equal [ "treatmenta", "treatmentb" ]
            , test "CSV column with all items" <|
                \_ ->
                    Tidy.strColumn "Jane Doe" table1CSV |> Expect.equal [ "16", "11" ]
            , test "CSV column with missing items" <|
                \_ ->
                    Tidy.strColumn "John Smith" table1CSV |> Expect.equal [ "", "2" ]
            , test "CSV numeric output of column with all items" <|
                \_ ->
                    Tidy.numColumn "Jane Doe" table1CSV |> Expect.equal [ 16, 11 ]
            , test "CSV numeric column with missing items" <|
                \_ ->
                    Tidy.numColumn "John Smith" table1CSV |> Expect.equal [ 0, 2 ]
            , test "CSV numeric conversion of non-numeric column" <|
                \_ ->
                    Tidy.numColumn "Treatment" table1CSV |> Expect.equal [ 0, 0 ]
            , test "TSV simple table" <|
                \_ ->
                    table1TSV |> Expect.equal table1
            , test "ragged CSV" <|
                \_ ->
                    Tidy.fromCSV raggedCSV |> Expect.equal raggedTable
            , test "Unruly CSV" <|
                \_ ->
                    Tidy.fromCSV complexCSV |> Expect.equal complexTable
            ]
        , describe "fromGrid"
            [ test "gridText" <|
                \_ ->
                    gridTextTable |> Expect.equal gridTable
            , test "gridRows" <|
                \_ ->
                    gridRowsTable |> Expect.equal gridTable
            , test "gridTextRagged" <|
                \_ ->
                    gridTextRaggedTable |> Expect.equal gridTableRagged
            , test "gridRowsRagged" <|
                \_ ->
                    gridRowsRaggedTable |> Expect.equal gridTableRagged
            ]
        , describe "programaticTables"
            [ test "addingColumns" <|
                \_ ->
                    table1CSV |> Expect.equal table2
            , test "addingRows" <|
                \_ ->
                    table1CSV |> Expect.equal table3
            , test "transposing" <|
                \_ ->
                    table4 |> Expect.equal (Tidy.transposeTable "Treatment" "Person" table2)
            ]
        , describe "leftJoins"
            [ test "leftJoinDiffKey" <|
                \_ ->
                    Tidy.leftJoin ( jTable1DiffKey, "Key1" ) ( jTable2DiffKey, "Key2" ) |> Expect.equal ljTableDiffKey
            , test "leftJoinSameKey" <|
                \_ ->
                    Tidy.leftJoin ( jTable1SameKey, "Key" ) ( jTable2SameKey, "Key" ) |> Expect.equal ljTableSameKey
            , test "leftJoinReflexive" <|
                \_ ->
                    Tidy.leftJoin ( jTable1SameKey, "Key" ) ( jTable1SameKey, "Key" ) |> Expect.equal jTable1SameKey
            , test "leftJoinUnmatchedKey1" <|
                \_ ->
                    Tidy.leftJoin ( jTable1SameKey, "Unmatched" ) ( jTable2SameKey, "Key" ) |> Expect.equal jTable1SameKey
            , test "leftJoinUnmatchedKey2" <|
                \_ ->
                    Tidy.leftJoin ( jTable1SameKey, "Key" ) ( jTable2SameKey, "Unmatched" ) |> Expect.equal jTable1SameKey
            ]
        , describe "rightjoins"
            [ test "rightJoinDiffKey" <|
                \_ ->
                    Tidy.rightJoin ( jTable1DiffKey, "Key1" ) ( jTable2DiffKey, "Key2" ) |> Expect.equal rjTableDiffKey
            , test "rightJoinSameKey" <|
                \_ ->
                    Tidy.rightJoin ( jTable1SameKey, "Key" ) ( jTable2SameKey, "Key" ) |> Expect.equal rjTableSameKey
            , test "rightJoinReflexive" <|
                \_ ->
                    Tidy.rightJoin ( jTable1DiffKey, "Key1" ) ( jTable1DiffKey, "Key1" ) |> Expect.equal jTable1DiffKey
            , test "rightJoinUnmatchedKey1" <|
                \_ ->
                    Tidy.rightJoin ( jTable1SameKey, "Unmatched" ) ( jTable2SameKey, "Key" ) |> Expect.equal jTable2SameKey
            , test "rightJoinUnmatchedKey2" <|
                \_ ->
                    Tidy.rightJoin ( jTable1SameKey, "Key" ) ( jTable2SameKey, "Unmatched" ) |> Expect.equal jTable2SameKey
            ]
        , describe "innerjoins"
            [ test "innerJoinDiffKey" <|
                \_ ->
                    Tidy.innerJoin "NewKey" ( jTable1DiffKey, "Key1" ) ( jTable2DiffKey, "Key2" ) |> Expect.equal ijTable
            , test "innerJoinSameKey" <|
                \_ ->
                    Tidy.innerJoin "NewKey" ( jTable1SameKey, "Key" ) ( jTable2SameKey, "Key" ) |> Expect.equal ijTable
            , test "innerJoinReflexive" <|
                \_ ->
                    Tidy.innerJoin "Key" ( jTable1SameKey, "Key" ) ( jTable1SameKey, "Key" ) |> Expect.equal jTable1SameKey
            , test "innerJoinUnmatchedKey1" <|
                \_ ->
                    Tidy.innerJoin "NewKey" ( jTable1SameKey, "Unmatched" ) ( jTable2SameKey, "Key" ) |> Expect.equal Tidy.empty
            , test "innerJoinUnmatchedKey2" <|
                \_ ->
                    Tidy.innerJoin "NewKey" ( jTable1SameKey, "Key" ) ( jTable2SameKey, "Unmatched" ) |> Expect.equal Tidy.empty
            ]
        , describe "outerjoins"
            [ test "outerJoinDiffKey" <|
                \_ ->
                    Tidy.outerJoin "NewKey" ( jTable1DiffKey, "Key1" ) ( jTable2DiffKey, "Key2" ) |> Expect.equal ojTable
            , test "outerJoinSameKey" <|
                \_ ->
                    Tidy.outerJoin "NewKey" ( jTable1SameKey, "Key" ) ( jTable2SameKey, "Key" ) |> Expect.equal ojTable
            , test "outerJoinReflexive" <|
                \_ ->
                    Tidy.outerJoin "Key" ( jTable1SameKey, "Key" ) ( jTable1SameKey, "Key" ) |> Expect.equal jTable1SameKey
            , test "outerJoinUnmatchedKey1" <|
                \_ ->
                    Tidy.outerJoin "NewKey" ( jTable1SameKey, "Unmatched" ) ( jTable2SameKey, "Key" ) |> Expect.equal Tidy.empty
            , test "outerJoinUnmatchedKey2" <|
                \_ ->
                    Tidy.outerJoin "NewKey" ( jTable1SameKey, "Key" ) ( jTable2SameKey, "Unmatched" ) |> Expect.equal Tidy.empty
            ]
        , describe "diffTables"
            [ test "leftDiffDiffKey" <|
                \_ ->
                    Tidy.leftDiff ( jTable1DiffKey, "Key1" ) ( jTable2DiffKey, "Key2" ) |> Expect.equal ldTableDiffKey
            , test "leftDiffSameKey" <|
                \_ ->
                    Tidy.leftDiff ( jTable1SameKey, "Key" ) ( jTable2SameKey, "Key" ) |> Expect.equal ldTableSameKey
            , test "rightDiffDiffKey" <|
                \_ ->
                    Tidy.rightDiff ( jTable1DiffKey, "Key1" ) ( jTable2DiffKey, "Key2" ) |> Expect.equal rdTableDiffKey
            , test "rightDiffSameKey" <|
                \_ ->
                    Tidy.rightDiff ( jTable1SameKey, "Key" ) ( jTable2SameKey, "Key" ) |> Expect.equal rdTableSameKey
            , test "leftDiffUnmatchedKey1" <|
                \_ ->
                    Tidy.leftDiff ( jTable1SameKey, "Unmatched" ) ( jTable2SameKey, "Key" ) |> Expect.equal Tidy.empty
            , test "leftDiffUnmatchedKey2" <|
                \_ ->
                    Tidy.leftDiff ( jTable1SameKey, "Key" ) ( jTable2SameKey, "Unmatched" ) |> Expect.equal jTable1SameKey
            , test "leftDiffUnmatchedBothKeys" <|
                \_ ->
                    Tidy.leftDiff ( jTable1SameKey, "Unmatched" ) ( jTable2SameKey, "Unmatched" ) |> Expect.equal Tidy.empty
            , test "rightDiffUnmatchedKey1" <|
                \_ ->
                    Tidy.rightDiff ( jTable1SameKey, "Unmatched" ) ( jTable2SameKey, "Key" ) |> Expect.equal jTable2SameKey
            , test "rightDiffUnmatchedKey2" <|
                \_ ->
                    Tidy.rightDiff ( jTable1SameKey, "Key" ) ( jTable2SameKey, "Unmatched" ) |> Expect.equal Tidy.empty
            , test "rightDiffUnmatchedBothKeys" <|
                \_ ->
                    Tidy.rightDiff ( jTable1SameKey, "Unmatched" ) ( jTable2SameKey, "Unmatched" ) |> Expect.equal Tidy.empty
            ]
        , describe "columnMapping"
            [ test "noBlanksMap" <|
                \_ ->
                    Tidy.mapColumn "treatmenta" impute table4 |> Expect.equal table4NoBlanks
            , test "identityMap" <|
                \_ ->
                    Tidy.mapColumn "colB" identity jTable1SameKey |> Expect.equal jTable1SameKey
            , test "missingcol" <|
                \_ ->
                    Tidy.mapColumn "nonExistentColumn" (always "Should never happen") jTable2DiffKey |> Expect.equal jTable2DiffKey
            ]
        , describe "bisecting"
            [ test "bisectedGrid" <|
                \_ ->
                    Tidy.bisect "z" zBisect ( "zr", "zc" ) gridTable |> Expect.equal gridTableBisected
            ]
        , describe "spreadingAndGathering"
            [ test "tableFromJSON" <|
                \_ ->
                    tidyTreatmentJson |> Expect.equal tidyTreatment
            , test "spread" <|
                \_ ->
                    Tidy.spread "readingType" "temperature" gatherTable |> Expect.equal spreadTable
            , test "gatherTemperatures" <|
                \_ ->
                    Tidy.gather "readingType" "temperature" [ ( "minTemp", "minTemp" ), ( "maxTemp", "maxTemp" ) ] spreadTable |> Expect.equal gatherTable
            ]
        , describe "normalization"
            [ test "normaizeRedundant1" <|
                \_ ->
                    Tidy.normalize "id" [ "animal", "name", "age" ] redundantTable |> Expect.equal ( keyTable, valueTable )
            , test "normaizeRedundant2" <|
                \_ ->
                    Tidy.normalize "id" [ "name", "animal", "age" ] redundantTable |> Expect.equal ( keyTable, valueTable )
            , test "normaizeRedundant3" <|
                \_ ->
                    Tidy.normalize "id" [ "age", "name", "animal" ] redundantTable |> Expect.equal ( keyTable, valueTable )
            , test "normaizeRedundantNoKeys" <|
                \_ ->
                    Tidy.normalize "id" [] redundantTable |> Expect.equal ( Tidy.empty, redundantTable )
            , test "normaizeRedundantNoValidKeys" <|
                \_ ->
                    Tidy.normalize "id" [ "x", "y", "z" ] redundantTable |> Expect.equal ( Tidy.empty, redundantTable )
            , test "normaizeAndJoin" <|
                \_ ->
                    let
                        ( kt, vt ) =
                            Tidy.normalize "id" [ "animal", "name", "age" ] redundantTable
                    in
                    Tidy.rightJoin ( kt, "id" ) ( vt, "id" )
                        |> Tidy.removeColumn "id"
                        |> Expect.equal redundantTable
            ]
        ]


impute : String -> String
impute val =
    if val == "" then
        "0"

    else
        val


zBisect : String -> ( String, String )
zBisect val =
    ( String.left 2 val, String.left 1 val ++ String.right 1 val )
