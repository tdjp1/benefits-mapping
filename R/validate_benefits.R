#' Validate a benefits structure
#'
#' This function checks that a valid benefits tibble has been supplied
#'
#' @param benefits Benefits tibble
#'
#' @return logical
#'
#' @examples
#' b <- create_benefits()
#' if (validate_benefits(b)) print("Benefits Valid")
#
#' @export
#'
validate_benefits <- function(benefits) {
    # Make sure we have good input
    status <- TRUE
    if (!tibble::is_tibble(benefits) && !is.data.frame(benefits)) {
        warning("benefits is not a tibble or a data frame")
        status <- FALSE
        return(status)
    }

    # Check for essential columns
    cols <- colnames(benefits)
    links <- cols[grepl("^LeadsTo[0-9]+$", cols)]

    # Check for required columns
    for (c in required_info) {
        if (!c %in% cols) {
            warning("Required column ", c, " missing")
            status <- FALSE
        }
    }

    # Check that all columns are allowed
    for (c in cols) {
        if (!c %in% c(links, required_info, optional_info)) {
            warning("Invalid column ", c, " found")
            status <- FALSE
        }
    }

    return(status)
}
