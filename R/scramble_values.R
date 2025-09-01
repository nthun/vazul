#' The function scrambles the values of a single variable
#'
#' @param x a vector `x`
#' @return the scrambled vector
#' #' @examples
#'
#' x <- letters[1:10]
#' scramble_values(x)
#'
#' # Calculate mean age of coffee lovers
#' calculate_group_mean(example_data, "Age", "Likes_Coffee")
#'
#'
#' @export
scramble_values <- function(x) {

    sample(x, length(x), replace = FALSE)

}
