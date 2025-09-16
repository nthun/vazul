# Register global variables used in dplyr/tidyeval contexts
# This suppresses R CMD check notes about undefined variables

utils::globalVariables(c(".row_id"))

# Ensure 'utils' is visibly used (avoids false "not imported" NOTE)
if (FALSE) utils::tar

