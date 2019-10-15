# Tidy

_Tidying tabular data so you don't have to._

[![elm version](https://img.shields.io/badge/Elm-v0.19-blue.svg?style=flat-square)](http://elm-lang.org)
[![Contributor Covenant](https://img.shields.io/badge/Contributor%20Covenant-v1.4%20adopted-ff69b4.svg)](CODE_OF_CONDUCT.md)

## Data Shaping and Tidying

Leaning heavily on the principles of the [tidyverse](https://www.tidyverse.org), and especially [tidy data](https://tidyr.tidyverse.org/articles/tidy-data.html), this package makes it easy to reshape and tidy tabular data for easier data analysis and visualization.

Use cases include:

- Importing tabular data from a CSV file for analysis.
- Joining and normalizing relational tables for data query.
- Editing, filtering and mapping rows and columns of data in a table.
- Creating _tidy_ data for visualization with [elm-vegalite](https://package.elm-lang.org/packages/gicentre/elm-vegalite/latest/), [elm-vega](https://package.elm-lang.org/packages/gicentre/elm-vega/latest/), [elm-visualization](https://package.elm-lang.org/packages/gampleman/elm-visualization/latest) and other visualization packages.
- Building data science applications in Elm.

Tidy data are tables of data where

- each _variable_ is a column.
- each _observation_ is a row.
- each _value_ is a cell.

The reality faced by many trying to work with datasets is that they are not in this format. The _tidy_ package allows you to transform data into tidy tables so you spend less time fighting with data tools and more time working on your analysis and visualization.
