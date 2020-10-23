# Tidy Changes

# Pending changes

## Additions

- `insertSetIndexColumn` for partitioning a column's values into sets and assigning a unique id to each. Useful for spreading a table comprising only key and value columns.

### Bug Fixes

- Better handling of ragged tables. Some transposing operations that encountered empty columns previously resulted in empty tables; they now transpose all non empty rows and columns. All ragged tables now padded with empty strings.

### Other Changes

---
