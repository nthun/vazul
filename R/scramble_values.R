#' The function scrambles the values of a single variable
#'
#' @param x a vector `x`
#' @return the scrambled vector
#' @examples
#'
#' x <- letters[1:10]
#' scramble_values(x)
#'
#' # Example with numeric vector
#' nums <- 1:5
#' scramble_values(nums)
#'
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

    sample(x, length(x), replace = FALSE)

}
