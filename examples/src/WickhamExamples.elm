module WickhamExamples exposing (main)

import Browser
import Html exposing (Html, div, h1, h2, hr, text)
import Markdown
import Tidy exposing (..)


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
        , messy4 |> tableSummary -1 |> toHtml
        , h2 [] [ text "Tidied table  (table 6)" ]
        , tidy6 |> tableSummary -1 |> toHtml
        , hr [] []
        , h2 [] [ text "Messy table (table 7)" ]
        , messy7 |> tableSummary -1 |> toHtml
        , h2 [] [ text "Tidied table (table 8)" ]
        , tidy8 |> tableSummary -1 |> toHtml
        , hr [] []
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
        |> melt "Treatment" "result" [ ( "treatmenta", "a" ), ( "treatmentb", "b" ) ]


messy4 : Table
messy4 =
    """
Religion,<$10k,$10-20k,$20-30k,$30-40k,$40-50k,$50-75k,$75-100k,$100-150k,>150k,Don't know/refused
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
        |> melt "Income"
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
year, artist,           track,                  time, date.entered, wk1, wk2, wk3
2000, 2 Pac,            Baby Don't Cry,         4:22, 2000-02-26,   87,  82,  72
2000, 2Ge+her,          The Hardest Part Of..., 3:15, 2000-09-02,   91,  87,  92
2000, 3 Doors Down,     Kryptonite,             3:15, 2000-04-08,   81,  70,  68
2000, 98^0,             Give Me Just One Night, 3:24, 2000-08-19,   51,  39,  34
2000, A*Teens,          Dancing Queen,          3:44, 2000-07-08,   97,  97,  96
2000, Aaliyah,          I Don't Wanna,          4:15, 2000-01-29,   84,  62,  51
2000, Aaliyah,          Try Again,              4:03, 2000-03-18,   59,  53,  38
2000, "Adams, Yolanda", Open My Heart,          5:30, 2000-08-26,   76,  76,  74
"""
        |> fromCSV


tidy8 : Table
tidy8 =
    messy7
        |> melt "week"
            "rank"
            [ ( "wk1", "1" )
            , ( "wk2", "2" )
            , ( "wk3", "3" )
            ]
        |> filterColumns ((/=) "date.entered")


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
