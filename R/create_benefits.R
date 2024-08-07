#' Create a skeleton benefits mapping structure
#'
#' This function makes it easier to get started with creating a benefits map by
#' creating a template data frame that can be edited or exported. Each row
#' include one element at one level (eg an output, outcome, benefit or impact) and
#' describes which level it is at and which element it leads to at the next level.
#' The function also creates a number of LeadsTo columns that shows for each element
#' which element in the next level is dependent on that particular element. This is
#' created with a complete combinatorial connection.
#'
#' @param count Number of elements (benefits) for each level in the table. This can
#' be either an integer, in which case the same number number of elements is
#' created at each level, vector of the same length as levels to get different
#' numbers of elements at each level
#' @param levels Vector of Titles for level, or "PMI" as shorthand for
#' c("Output",
#'   "Outcome",
#'   "Intermediate benefit",
#'   "End benefit",
#'   "Impact")
#'
#' @return benefits with standard columns, as a tibble
#'
#' @examples
#' b <- create_benefits()
#' b
#' b <- create_benefits(count = c(2, 4, 1),
#'                      levels = c("What we will do",
#'                                "What will happen",
#'                                "Why should anyone care"))
#' b
#' b <- create_benefits(count = c(1, 2, 1),
#'                      levels = c("Assumption",
#'                                 "Initiative",
#'                                 "Outcome"))
#
#' @export
#'
create_benefits <- function(count = 1,
                            levels = c("Activity",
                                       "Outcome",
                                       "Business change",
                                       "Benefit",
                                       "Objective",
                                       "Value")) {

    # Check any level shorcuts are used
    if (length(levels) == 1 &&
        levels == "PMI") {
        levels = c("Output",
                   "Outcome",
                   "Intermediate benefit",
                   "End benefit",
                   "Impact")
    }

    # Make sure count is numeric
    if (!is.numeric(count)) stop("count needs to be numeric")

    # Make sure count is a single number or matches length of levels
    if (length(count) != 1 &&
        length(count) != length(levels))
        stop("count must be a number or a vector the same length as levels")


    # Setup counts_this array
    if (length(count) == 1) {
        counts_this <- rep(count, times = length(levels))
    } else {
        counts_this <- count
    }

    # Create tibble based on the requirements
    df <- tibble::tibble(Level = rep(levels, times = counts_this))

    # Build Elements column with numbered element matching the level
    elements <- rep(levels, times = counts_this)
    ex <- as.integer()
    for (i in count) {
        ex <- c(ex, 1:i)
    }
    elements <- paste(elements, ex)

    # Add Elements to the tibble
    df$Element <- elements


    # Counts_next used to build LeadsTo
    counts_next <- c(counts_this[2:length(counts_this)], NA)

    # Create all reqiured LeadsTo columns
    for (i in 1:max(counts_this)) {
        col <- paste0("LeadsTo", i)
        df[[col]] <- as.integer(NA)
    }

    # Build information needed to populate LeadsTo columns
    links <- as.integer()
    for (i in 1:length(counts_next)) {
        links <- c(links, rep(counts_next[i], times = counts_this[i]))
    }

    # Enter links into the LeadsTo columns
    for (i in 1:nrow(df)) {
        if (is.na(links[i])) next()
        for (j in 1:links[i]) {
            col <- paste0("LeadsTo", j)
            df[[col]][i] <- j
        }
    }

    # Excplicit return
    return(df)
}
