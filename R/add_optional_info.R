#' Add the next LeadsTo column
#'
#' Add a new LeadsTo column. A new benefits structure from create_benefits has
#' a column labelled LeadsTo1 which indicates which benefit/or impact it contributes
#' to in the next group. If a benefit leads to more than more benefit in the next
#' group this needs to be recorded in a new column.
#'
#' @param benefits Benefits tibble
#' @param add vector of columns to add (including LeadsTo and Measure)
#'
#' @return benefits structure
#'
#' @examples
#' b <- create_benefits()
#' b <- add_optional_info(b, add = "LeadsTo")
#
#' @export
#'
add_optional_info <- function(benefits, add) {
    # Check we got a valid benefits structure
    if (!validate_benefits(benefits)) stop("Invalid benefits structure")
    reorder <- FALSE

    # Check all elements in add are valid
    for (a in add) {
        if (!a %in% c("LeadsTo", optional_info)) stop("Invalid optional column ", a)
    }

    # Add optional_info elements from add if not already present
    # quietly ignore if the column is already present
    cols <- colnames(benefits)
    for (a in optional_info) {
        if (a %in% add && !a %in% cols) {
            benefits[[a]] <- as.character(NA)
            reorder <- TRUE
        }
    }

    # Special case for next LeadsTo column
    if ("LeadsTo" %in% add) {
        # Check which LeadsTo columns exist in cols
        links <- cols[grepl("LeadsTo[0-9]+$", cols)]
        last_link <- max(as.numeric(sub("LeadsTo", "", links)))

        # Name of new column
        new_col <- paste0("LeadsTo", last_link + 1)

        # Add new colummn
        benefits[[new_col]] <- as.integer(NA)
    }

    if (reorder) {
        cols <- colnames(benefits)
        links <- cols[grepl("^LeadsTo[0-9]+$", cols)]
        notlinks <- cols[!cols %in% links]
        benefits <- benefits[c(notlinks, links)]
    }

    # Return benefits structure
    return(benefits)
}
