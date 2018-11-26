module WickhamExamples exposing (main)

import Browser
import Html exposing (Html, div, h1, h2, hr, text)
import Html.Events exposing (onClick)
import Markdown
import Tidy exposing (..)


main =
    Browser.sandbox { init = 0, update = update, view = view }


update msg model =
    model


view model =
    div []
        [ h1 [] [ text "Examples from Wickham (2014)" ]
        , h2 [] [ text "Messy Table 1" ]
        , messy1 |> tableSummary -1 |> toHtml
        , h2 [] [ text "Tidy Table 3" ]
        , tidy3 |> tableSummary -1 |> toHtml
        , hr [] []
        , h2 [] [ text "Messy Table 4" ]
        , messy4 |> tableSummary -1 |> toHtml
        , h2 [] [ text "Tidy Table 6" ]
        , tidy6 |> tableSummary -1 |> toHtml
        ]


messy1 : Table
messy1 =
    """Person,treatmenta,treatmentb
John Smith, , 2
Jane Doe, 16, 11
Mary Johnson, 3,
"""
        |> fromCSV


tidy3 : Table
tidy3 =
    messy1
        |> melt "Treatment"
            "result"
            [ ( "treatmenta", "a" )
            , ( "treatmentb", "b" )
            ]


messy4 : Table
messy4 =
    """
Religion,income0-10k,income10-20k,income20-30k,income30-40k,income40-50k,income50-75k
Agnostic,27,34,60,81,76,137
Atheist,12,27,37,52,35,70
Buddhist,27,21,30,34,33,58
Catholic,418,617,732,670,638,1116
Don't know/refused,15,14,15,11,10,35
Evangelical Prot,575,869,1064,982,881,1486
Hindu,1,9,7,9,11,34
Historically Black Prot,228,224,236,238,197,223
Jehovah's Witness,20,27,24,24,21,30
Jewish,19,19,25,25,30,95
"""
        |> fromCSV


tidy6 : Table
tidy6 =
    messy4
        |> melt "Income"
            "frequency"
            [ ( "income0-10k", "$0-10k" )
            , ( "income10-20k", "$10-20k" )
            , ( "income20-30k", "$20-30k" )
            , ( "income30-40k", "$30-40k" )
            , ( "income40-50k", "$40-50k" )
            , ( "income50-75k", "$50-75k" )
            ]


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
