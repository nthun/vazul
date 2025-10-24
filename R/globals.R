# Register global variables used in dplyr/tidyeval contexts
# This suppresses R CMD check notes about undefined variables

utils::globalVariables(c(".row_id"))
utils::globalVariables(c(".scrambled_rows"))
utils::globalVariables(c("evaluate_as_tidyselect", "try_evaluate_as_character"))


