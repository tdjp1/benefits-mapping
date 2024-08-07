#' Validate a benefits structure
#'
#' This function tidies a benefits map structure. Lines with blank Level or
#' Element are skipped, LeadsTo are converted back to integer
#'
#' @param benefits Benefits tibble
#'
#' @return Benefits tibble
#'
#' @examples
#' b <- clean_benefits()
#
#' @export
#'
clean_benefits <- function(benefits) {
    # Remove any blank lines
    empty <- is.na(benefits$Level) | benefits$Level == "" |
                       is.na(benefits$Element) | benefits$Element == ""
    benefits <- benefits[!empty,]

    # Reset all LeadsTo columns to be integer
    cols <- names(benefits)
    for (c in cols) {
        if (!grepl("LeadsTo", c)) next
        benefits[[c]] <- as.integer(benefits[[c]])
    }

    # Check we got a valid benefits structure
    if (!validate_benefits(benefits)) stop("Invalid benefits structure")
    return(benefits)
}
