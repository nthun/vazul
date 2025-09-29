# Register global variables used in dplyr/tidyeval contexts
# This suppresses R CMD check notes about undefined variables

utils::globalVariables(c(".row_id"))
utils::globalVariables(c(".scrambled_rows"))

# Ensure 'utils' is visibly used (avoids false "not imported" NOTE)
# Use a condition that is not statically FALSE
utils::capture.output(NULL)

