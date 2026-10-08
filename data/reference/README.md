# Reference files

## Steel categories (optional)

The **Steel** tab appears only when a file named `steel_categories.csv` is in
this folder.

Format: one column per steel category, with the category name in the header row
and that category's 6-digit HS codes listed underneath. Columns can be different
lengths. See `steel_categories_TEMPLATE.csv` for an example.

To set it up:

1. Copy `steel_categories_TEMPLATE.csv` (or your existing
   `Steel Product Categories.csv`) to `steel_categories.csv` in this folder.
2. Edit it in Excel or any text editor and save it as CSV.
3. Restart the Trade Explorer.

Leading zeros dropped by Excel are fine: codes are padded back to six digits.
