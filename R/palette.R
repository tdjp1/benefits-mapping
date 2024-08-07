#' expand_palette
#' Expand an vector of colours to be a specified length by repeating
#'
#' @param palette vector of colours to expand
#' @param n number of colours requested
#' @return colours vector of colours in hex-format
#'
expand_palette <- function(palette, n = 5) {
    if (n < 0) stop("Positive number of colours must be requested")
    v <- vector(length = n, mode = "character")
    colours <- paste0(v, palette)[1:n]
    return(colours)
}
