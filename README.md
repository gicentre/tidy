# Tidy

_Tidying tabular data so you don't have to._

## Data Shaping and Tidying

Leaning heavily on the principles of the [tidyverse](https://www.tidyverse.org), this package makes it easy to reshape and tidy tabular data for data analysis and visualization.

Use cases include:

- Importing tabular data from a CSV file for analysis.
- Joining relational tables for data query.
- Editing, filtering and mapping rows and columns of data in a table.
- Creating _tidy_ data for visualization with [elm-vegalite](https://package.elm-lang.org/packages/gicentre/elm-vegalite/latest/), [elm-vega](https://package.elm-lang.org/packages/gicentre/elm-vega/latest/), [elm-visualization](https://package.elm-lang.org/packages/gampleman/elm-visualization/latest) and other visualization packages.
- Building data science applications in Elm.

Tidy data are tables of data where

- each _variable_ is in a column.
- each _observation_ is a row.
- each _value_ is a cell.

Unfortunately, many datasets are not in this format; the _tidy_ package allows you to transform data into tidy tables so you
spend less time fighting with data tools and more time working on your analysis and visualization. For a more complete description of tidy data, see [Wickham (2014)](https://www.jstatsoft.org/index.php/jss/article/view/v059i10/v59i10.pdf).
