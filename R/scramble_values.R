#' Scramble a vector of values
#'
#' @param x a vector `x`
#' @return the scrambled vector
#' @examples
#'
#' # Example with character vector
#' set.seed(123)
#'
#' x <- letters[1:10]
#' scramble_values(x)
#'
#'#' # Example with numeric vector
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
    # Input validation - check if x is a vector
    if (is.null(x)) {
        stop("Input 'x' cannot be NULL. Please provide a vector.", call. = FALSE)
    }

    if (!is.vector(x)) {
        stop("Input 'x' must be a vector. Received object of class: ",
             paste(class(x), collapse = ", "), ".", call. = FALSE)
    }

    if (length(x) == 0) {
        stop("Input 'x' cannot be an empty vector. Please provide a vector with at least one element.", call. = FALSE)
    }

    # Handle the special case where length(x) == 1
    # This prevents R's sample() from treating a single integer as 1:x
    if (length(x) == 1) {
        return(x)
    }

    sample(x, length(x), replace = FALSE)

}
