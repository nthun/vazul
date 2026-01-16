#' Scramble a vector of values
#' @keywords scramble
#' @param x a vector `x`
#' @return the scrambled vector
#'
#' @seealso \code{\link{scramble_variables}} for scrambling multiple variables in a data frame, and
#' \code{\link{scramble_variables_rowwise}} for rowwise scrambling.
#'
#' @examples
#'
#' # Example with character vector
#' set.seed(123)
#'
#' x <- letters[1:10]
#' scramble_values(x)
#'
#' # Example with numeric vector
#' nums <- 1:5
#' scramble_values(nums)
#'
#' # Scramble a column in the 'williams' dataset
#' data(williams)
#'
#' # Simple scrambling of a single column
#' set.seed(123)
#' williams$ecology_scrambled <- scramble_values(williams$ecology)
#' head(williams[c("ecology", "ecology_scrambled")])
#'
#' @export
scramble_values <- function(x) {
  validate_vector(x)

  # Handle the special case where length(x) == 1
  # This prevents R's sample() from treating a single integer as 1:x
  if (length(x) == 1) {
    return(x)
  }

  sample(x, length(x), replace = FALSE)
}
