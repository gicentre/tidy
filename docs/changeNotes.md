# Tidy Changes

## Pending changes

_none_

---

## V1.6.0

### V1.6 Additions

- `insertSetIndexColumn` for partitioning a column's values into sets and assigning a unique id to each. Useful for spreading a table comprising only key and value columns.

### V1.6 Bug Fixes

- Better handling of ragged tables. Some transposing operations that encountered empty columns previously resulted in empty tables; they now transpose all non empty rows and columns. All ragged tables now padded with empty strings.

### V1.6 Other Changes

- Some code cleanup to reduce download footprint.

---

## V1.5.0

### V1.5 Additions

- `splitAt` function for bisecting data values at any position.
- `disaggregate` for flexible disaggregation of data values using regular expressions.
- `normalize` for splitting a table into a key table and value table
- `moveColumnToEnd` for reordering of table columns

### V1.5 Refactoring

- Computation of `gather` and `rowFilter` operations now much more efficient and suitable for larger data tables.

### V1.5 Minor

- Minor improvements to API documentation.
- Table normalization added to examples.
- Additional tests for new functions.

---

## V1.4.0

### V1.4 Additions

- `toCSV` and `toDelimited` for converting a table into CSV and other text delimited strings. Useful when saving a table as a file.

### V1.4 Minor

- Minor improvements to API documentation.

---

## V1.3.0

### V1.3 Additions

- `headTail` convenience function for splitting a String into a tuple separating its first and remaining characters.

### V1.3 Minor

- Add example of multi column.-group gathering to API doc (uses `headTail` for more compact bisection of column values)

---

## V1.2.0

### V1.2 Additions

- `insertIndexColumn` for adding a column containing row-unique indices. Useful for relational joining.

---

## V1.1.0

### V1.1 Additions

- `insertColumnFromJson` to extract values from JSON arrays of objects as table columns.

### V1.1 Minor

- Minor improvements to API documentation formatting.

---

## V1.0.0 Initial Release

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

Unfortunately, many datasets are not in this format; the _tidy_ package allows you to transform data into tidy tables so you spend less time fighting with data tools and more time working on your analysis and visualization. For a more complete description of tidy data, see [Wickham (2014)](https://www.jstatsoft.org/index.php/jss/article/view/v059i10/v59i10.pdf).
