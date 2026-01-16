# Register global variables used in dplyr/tidyeval contexts
# This suppresses R CMD check notes about undefined variables

utils::globalVariables(c(".row_id"))
utils::globalVariables(c(".scrambled_rows"))

