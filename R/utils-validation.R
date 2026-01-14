# ──────────────────────────────────────────────────────────────────────────────
# Generic input validation helper functions
# ──────────────────────────────────────────────────────────────────────────────

#' Validate that input is a data frame
#'
#' Internal helper function for validating data frame input.
#'
#' @param data The input to validate.
#' @return NULL (invisibly). Throws an error if validation fails.
#' @keywords internal
#' @noRd
validate_data_frame <- function(data) {
  if (!is.data.frame(data)) {
    stop("Input 'data' must be a data frame. Received object of class: ",
         paste(class(data), collapse = ", "), ".", call. = FALSE)
  }
  invisible(NULL)
}

#' Validate that a data frame is not empty
#'
#' Internal helper function for validating that a data frame has rows.
#'
#' @param data The data frame to validate.
#' @return NULL (invisibly). Throws an error if validation fails.
#' @keywords internal
#' @noRd
validate_data_frame_not_empty <- function(data) {
  if (nrow(data) == 0) {
    stop("Input 'data' cannot be an empty data frame.", call. = FALSE)
  }
  invisible(NULL)
}