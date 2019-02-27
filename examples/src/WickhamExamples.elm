module WickhamExamples exposing (main)

import Browser
import Dict exposing (Dict)
import Html exposing (Html, div, h1, h2, h3, hr, text)
import Iso8601
import Markdown
import Tidy exposing (..)
import Time


main =
    Browser.sandbox { init = 0, update = update, view = view }


update msg model =
    model


view model =
    div []
        [ h2 [] [ text "Messy table (table 1)" ]
        , messy1 |> tableSummary -1 |> toHtml
        , h2 [] [ text "Transposed messy table (table 2)" ]
        , messy2 |> tableSummary -1 |> toHtml
        , h2 [] [ text "Tidied table (table 3)" ]
        , tidy3 |> tableSummary -1 |> toHtml
        , hr [] []
        , h2 [] [ text "Messy table (table 4)" ]
        , div [] [ text "Column headings are values, not variable names." ]
        , messy4 |> tableSummary -1 |> toHtml
        , h2 [] [ text "Tidied table  (table 6)" ]
        , tidy6 |> tableSummary 10 |> toHtml
        , hr [] []
        , h2 [] [ text "Messy table (table 7)" ]
        , messy7 |> tableSummary -1 |> toHtml
        , h2 [] [ text "Tidied table (table 8)" ]
        , div [] [ text "Date is calculated by adding week to date.entered from the messy table. Ranks outside the top 100 filtered out." ]
        , tidy8 |> tableSummary 15 |> toHtml
        , hr [] []
        , h2 [] [ text "Messy table (table 9)" ]
        , messy9 |> tableSummary -1 |> toHtml
        , h2 [] [ text "Gathered table (table 10a)" ]
        , div [] [ text "First, the mixed gender/age columns are gathered and rows with missing data removed:" ]
        , gathered10a |> tableSummary 15 |> toHtml
        , h2 [] [ text "Tidy table (table 10b)" ]
        , div [] [ text "Then the values in the mixed gender/age column are split into two (sex and age)" ]
        , tidy10b |> tableSummary 15 |> toHtml
        , hr [] []
        , h2 [] [ text "Messy table (table 11)" ]
        , messy11 |> tableSummary -1 |> toHtml
        , h2 [] [ text "Gathered table (table 12a)" ]
        , div [] [ text "First, days of the month are gathered, converted into dates by combining with year and month values:" ]
        , gathered12a |> tableSummary 10 |> toHtml
        , h2 [] [ text "Spread table (table 12b)" ]
        , div [] [ text "Then the tmin/tmax temperatures are spread to create the tidy table.:" ]
        , gathered12b |> tableSummary 10 |> toHtml
        ]


messy1 : Table
messy1 =
    """Person,treatmenta,treatmentb
John Smith, , 2
Jane Doe, 16, 11
Mary Johnson, 3, 1
"""
        |> fromCSV


messy2 : Table
messy2 =
    messy1
        |> transposeTable "Person" "Treatment"


tidy3 : Table
tidy3 =
    messy1
        |> gather "Treatment" "result" [ ( "treatmenta", "a" ), ( "treatmentb", "b" ) ]


messy4 : Table
messy4 =
    """
religion,<$10k,$10-20k,$20-30k,$30-40k,$40-50k,$50-75k,$75-100k,$100-150k,>150k,Don't know/refused
Agnostic,                 27,  34,   60,  81,  76,  137, 122, 109,  84,   96
Atheist,                  12,  27,   37,  52,  35,   70,  73,  59,  74,   76
Buddhist,                 27,  21,   30,  34,  33,   58,  62,  39,  53,   54
Catholic,                418, 617,  732, 670, 638, 1116, 949, 792, 633, 1489
Don't know/refused,       15,  14,   15,  11,  10,   35,  21,  17,  18,  116
Evangelical Prot,        575, 869, 1064, 982, 881, 1486, 949, 723, 414, 1529
Hindu,                     1,   9,    7,   9,  11,   34,  47,  48,  54,   37
Historically Black Prot, 228, 224,  236, 238, 197,  223, 131,  81,  78,  339
Jehovah's Witness,        20,  27,   24,  24,  21,   30,  15,  11,   6,   37
Jewish,                   19,  19,   25,  25,  30,   95,  69,  87, 151,  162
"""
        |> fromCSV


tidy6 : Table
tidy6 =
    messy4
        |> gather "income"
            "freq"
            [ ( "<$10k", "<$10k" )
            , ( "$10-20k", "$10-20k" )
            , ( "$20-30k", "$20-30k" )
            , ( "$30-40k", "$30-40k" )
            , ( "$40-50k", "$40-50k" )
            , ( "$50-75k", "$50-75k" )
            , ( "$75-100k", "$75-100k" )
            , ( "$100-150k", "$100-150k" )
            , ( ">150k", ">150k" )
            , ( "Don't know/refused", "Don't know/refused" )
            ]


messy7 : Table
messy7 =
    """
year,artist,track,time, date.entered,wk1,wk2,wk3,wk4,wk5,wk6,wk7
2000,2 Pac,Baby Don't Cry,4:22, 2000-02-26,87,82,72,77,87,94,99
2000,2Ge+her,The Hardest Part Of...,3:15,2000-09-02,91,87,92,,,,
2000,3 Doors Down,Kryptonite,3:53,2000-04-08,81,70,68,67,66,57,54
2000,98^0,Give Me Just One Night,3:24,2000-08-19,51,39,34,26,26,19,2
2000,A*Teens,Dancing Queen,3:44, 2000-07-08,97,97,96,95,100,,
2000,Aaliyah,I Don't Wanna,4:15, 2000-01-29,84,62,51,41,38,35,35
2000,Aaliyah,Try Again,4:03, 2000-03-18,59,53,38,28,21,18,16
2000,"Adams, Yolanda",Open My Heart,5:30,2000-08-26,76,76,74,69,68,67,61
"""
        |> fromCSV


{-| Adds a given number of weeks to a date string in the form YYYY-MM-DD and
returns the new date in the same format. If the conversion of the date fails,
the original date string is returned.
-}
addWeek : Int -> String -> String
addWeek weekOffset baseDate =
    case Iso8601.toTime baseDate of
        Ok posixDate ->
            posixDate
                |> Time.posixToMillis
                -- 604800000 is number of milliseconds in a week
                |> (+) (weekOffset * 604800000)
                |> Time.millisToPosix
                |> Iso8601.fromTime
                |> String.left 10

        Err err ->
            baseDate


tidy8 : Table
tidy8 =
    let
        tidy =
            messy7
                |> gather "week"
                    "rank"
                    [ ( "wk1", "1" )
                    , ( "wk2", "2" )
                    , ( "wk3", "3" )
                    , ( "wk4", "4" )
                    , ( "wk5", "5" )
                    , ( "wk6", "6" )
                    , ( "wk7", "7" )
                    ]
                |> filterRows "rank" ((/=) "")

        dateColumn =
            List.map2
                (\wk -> addWeek (round wk - 1))
                (numColumn "week" tidy)
                (strColumn "date.entered" tidy)
    in
    tidy
        |> insertColumn "date.entered" dateColumn
        |> renameColumn "date.entered" "date"


messy9 : Table
messy9 =
    """country,year,m014,m1524,m2534,m3544,m4554,m5564,m65,mu,f014,f1524,f2534,f3544,f4554,f5564,f65,fu
AD,2000,0,0,1,0,0,0,0,,,,,,,,,
AE,2000,2,4,4,6,5,12,10,,3,16,1,3,0,0,4,
AF,2000,52,228,183,149,129,94,80,,93,414,565,339,205,99,36,
AG,2000,0,0,0,0,0,0,1,,1,1,1,0,0,0,0,
AL,2000,2,19,21,14,24,19,16,,3,11,10,8,8,5,11,
AM,2000,2,152,130,131,63,26,21,,1,24,27,24,8,8,4,
AN,2000,0,0,1,2,0,0,0,,0,0,1,0,0,1,0,
AO,2000,186,999,1003,912,482,312,194,,247,1142,1091,844,417,200,120,
AR,2000,97,278,594,402,419,368,330,,121,544,479,262,230,179,216,
AS,2000,,,,,1,1,,,,,,,1,,,
""" |> fromCSV


gathered10a : Table
gathered10a =
    messy9
        |> gather "column"
            "cases"
            ([ "m014", "m1524", "m2534", "m3544", "m4554", "m5564", "m65", "mu", "f014", "f1524", "f2534", "f3544", "f4554", "f5564", "f65", "fu" ]
                |> List.map (\s -> ( s, s ))
            )
        |> filterRows "cases" ((/=) "")


tidy10b : Table
tidy10b =
    let
        ageLookup =
            [ ( "014", "0-14" )
            , ( "1524", "15-24" )
            , ( "2534", "25-34" )
            , ( "3544", "35-44" )
            , ( "4554", "45-54" )
            , ( "5564", "55-64" )
            , ( "65", "65+" )
            ]
                |> Dict.fromList

        tidy =
            gathered10a
                |> bisect "column"
                    (\s ->
                        ( String.left 1 s
                        , Dict.get (String.dropLeft 1 s) ageLookup |> Maybe.withDefault "unknown"
                        )
                    )
                    ( "sex", "age" )

        cases =
            strColumn "cases" tidy
    in
    -- Not strictly necessary, but we move the 'cases' column to the end for consistency with paper:
    tidy |> removeColumn "cases" |> insertColumn "cases" cases


messy11 : Table
messy11 =
    """
id,year,month,element,d1,d2,d3,d4,d5,d6,d7,d8,d9,d10,d11,d12,d13,d14,d15,d16,d17,d18,d19,d20,d21,d22,d23,d24,d25,d26,d27,d28,d29,d30,d31
MX17004,2010,1,tmax,"","","","","","","","","","","","","","","","","","","","","","","","","","","","","",27.8,""
MX17004,2010,1,tmin,"","","","","","","","","","","","","","","","","","","","","","","","","","","","","",14.5,""
MX17004,2010,2,tmax,"",27.3,24.1,"","","","","","","",29.7,"","","","","","","","","","","",29.9,"","","","","","","",""
MX17004,2010,2,tmin,"",14.4,14.4,"","","","","","","",13.4,"","","","","","","","","","","",10.7,"","","","","","","",""
MX17004,2010,3,tmax,"","","","",32.1,"","","","",34.5,"","","","","",31.1,"","","","","","","","","","","","","","",""
MX17004,2010,3,tmin,"","","","",14.2,"","","","",16.8,"","","","","",17.6,"","","","","","","","","","","","","","",""
MX17004,2010,4,tmax,"","","","","","","","","","","","","","","","","","","","","","","","","","",36.3,"","","",""
MX17004,2010,4,tmin,"","","","","","","","","","","","","","","","","","","","","","","","","","",16.7,"","","",""
MX17004,2010,5,tmax,"","","","","","","","","","","","","","","","","","","","","","","","","","",33.2,"","","",""
MX17004,2010,5,tmin,"","","","","","","","","","","","","","","","","","","","","","","","","","",18.2,"","","",""
MX17004,2010,6,tmax,"","","","","","","","","","","","","","","","",28,"","","","","","","","","","","",30.1,"",""
MX17004,2010,6,tmin,"","","","","","","","","","","","","","","","",17.5,"","","","","","","","","","","",18,"",""
MX17004,2010,7,tmax,"","",28.6,"","","","","","","","","","",29.9,"","","","","","","","","","","","","","","","",""
MX17004,2010,7,tmin,"","",17.5,"","","","","","","","","","",16.5,"","","","","","","","","","","","","","","","",""
MX17004,2010,8,tmax,"","","","",29.6,"","",29,"","","","",29.8,"","","","","","","","","",26.4,"",29.7,"","","",28,"",25.4
MX17004,2010,8,tmin,"","","","",15.8,"","",17.3,"","","","",16.5,"","","","","","","","","",15,"",15.6,"","","",15.3,"",15.4
MX17004,2010,10,tmax,"","","","",27,"",28.1,"","","","","","",29.5,28.7,"","","","","","","","","","","","",31.2,"","",""
MX17004,2010,10,tmin,"","","","",14,"",12.9,"","","","","","",13,10.5,"","","","","","","","","","","","",15,"","",""
MX17004,2010,11,tmax,"",31.3,"",27.2,26.3,"","","","","","","","","","","","","","","","","","","","",28.1,27.7,"","","",""
MX17004,2010,11,tmin,"",16.3,"",12,7.9,"","","","","","","","","","","","","","","","","","","","",12.1,14.2,"","","",""
MX17004,2010,12,tmax,29.9,"","","","",27.8,"","","","","","","","","","","","","","","","","","","","","","","","",""
MX17004,2010,12,tmin,13.8,"","","","",10.5,"","","","","","","","","","","","","","","","","","","","","","","","",""
""" |> fromCSV


gathered12a : Table
gathered12a =
    let
        tidy =
            messy11
                |> gather "day" "value" (List.range 1 31 |> List.map (\n -> ( "d" ++ String.fromInt n, "0" ++ String.fromInt n |> String.right 2 )))

        dateColumn =
            List.map3
                (\yr mn dy -> yr ++ "-" ++ ("0" ++ mn |> String.right 2) ++ "-" ++ dy)
                (strColumn "year" tidy)
                (strColumn "month" tidy)
                (strColumn "day" tidy)
    in
    tidy
        |> removeColumn "month"
        |> removeColumn "day"
        |> insertColumn "year" dateColumn
        |> renameColumn "year" "date"


gathered12b : Table
gathered12b =
    gathered12a |> spread "element" "value"



--------------------------------------------------------------------------------


toHtml : List String -> Html msg
toHtml =
    String.concat
        >> Markdown.toHtmlWith
            { githubFlavored = Just { tables = True, breaks = False }
            , defaultHighlighting = Nothing
            , sanitize = False
            , smartypants = False
            }
            []
