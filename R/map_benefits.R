#' Create a DiagrammeR map of from a benefits structure
#'
#' Use the structure and just plot the boxes
#'
#' @param benefits Benefits tibble
#' @param title Title for the map
#' @param splines How edges are represented (line, spline, ortho or curved)
#' @param width Proportion of available width to use for element boxes
#' @param xscale Scale factor for x-dimension
#' @param yscale Scale factor for y-dimension
#' @param levels_textwrap String length for wrapping text in headers
#' @param elements_textwrap String length for wrapping text in benefits
#' @param fillcolour_benefit one or more colours to fill benefit elements
#' @param fontcolour_benefit one or more colours for text
#' @param fillcolour_disbenefit one or more colours to fill disbenefit elements
#' @param fontcolour_disbenefit one or more colours for all disbenefit text elements
#' @param fillcolour_measure Colour to fill any measures defined
#' @param fontcolour_measure Font colour for any measures
#' @param linkcolour_benefit colour for links for benefit elements
#' @param linkcolour_disbenefit colour for links for all disbenefit elements
#' @param fontname fontname for nodes and edges
#' @param fontname_title fontname for title
#' @param title_colour Font colour
#'
#' @return graph Graph that can be used by DiagrammeR for render or export
#'
#' @examples
#' b <- create_benefits()
#' b
#' g <- map_benefits(b)
#' DiagrammeR::render_graph(g)
#
#' @export
#'
map_benefits <- function(benefits,
                         title = NA,
                         splines = "line",
                         width = 0.75,
                         xscale = 1.5,
                         yscale = 1.0,
                         levels_textwrap = 12,
                         elements_textwrap = 15,
                         fillcolour_benefit = benefits_fill_colours,
                         fontcolour_benefit = benefits_text_colours,
                         fillcolour_disbenefit = disbenefits_fill_colour,
                         fontcolour_disbenefit = disbenefits_text_colour,
                         fillcolour_measure = measure_fill_colour,
                         fontcolour_measure = measure_text_colour,
                         linkcolour_benefit = "grey",
                         linkcolour_disbenefit = "grey",
                         fontname = "Calibri,Arial,Helvetica",
                         fontname_title = "Calisto MT,Times New Roman",
                         title_colour = "grey") {

    # Clean map and check it validates
    clean_benefits(benefits)

    # Find the classification from the levels using the first time each level
    # description appears to define the expected correct order
    headers <- unique(benefits$Level)
    nheaders <- length(headers)

    # Make palette right size for fill colours and font
    fillcolour_benefit <- expand_palette(benefits_fill_colours, nheaders)
    fontcolour_benefit <- expand_palette(benefits_text_colours, nheaders)

    # Make headers into a factor so we can sort first by group then by original order
    benefits$Factor <- factor(benefits$Level, levels = headers)
    benefits$Original <- 1:nrow(benefits)
    benefits$LevelID <- as.numeric(benefits$Factor)

    # Set colours for each benefit
    benefits$fillcolour <- NA
    benefits$fontcolour <- NA
    for (i in 1:nheaders) {
        update <- benefits$LevelID == i
        benefits$fillcolour[update] <- fillcolour_benefit[i]
        benefits$fontcolour[update] <- fontcolour_benefit[i]
    }

    # Sort
    benefits <- benefits[order(benefits$Factor, benefits$Original),]

    # Note Node ID for each benefit
    benefits$NodeID <- (nheaders + 1):(nheaders + nrow(benefits))

    # Wrap the text for the levels
    headers_wrapped <-
        unlist(lapply(
            strwrap(headers, width = levels_textwrap, simplify = FALSE),
            paste,
            collapse = "\n"
        )
    )

    # Remove quotes from headers
    headers_wrapped <- gsub("[\"']", "", headers_wrapped)

    # Set up a data frame for all benefits
    xy <- data.frame(label = benefits$Element)

    # Wrap the text for the elements
    xy$label <-
        unlist(lapply(
            strwrap(benefits$Element, width = elements_textwrap, simplify = FALSE),
            paste,
            collapse = "\n"
        )
    )

    # Remove quotes from elements
    xy$label <- gsub("[\"']", "", xy$label)

    # Get x-coordinate for each benefit using ID
    xy$x <- benefits$LevelID

    # Get number at each level
    counts <- unname(sapply(headers, function(h) {
        nrow(benefits[benefits$Level == h,])
    }))
    maxy <- max(counts) + 1

    # Sum up number of benefits in each group
    groups <- data.frame(count = counts)
    groups$sum <- NA
    for (i in 1:nrow(groups)) {
        groups$sum[i] <- sum(groups$count[1:i])
    }

    # Identify disbenefits to change colour of elements
    cols <- colnames(benefits)
    links <- cols[grepl("^LeadsTo[0-9]+$", cols)]

    # Check if any disbenefits - negative number in a LeadsTo column
    benefits$is_disbenefit <- FALSE

    for (link in links) {
        benefits$is_disbenefit[benefits[[link]] < -0] <- TRUE
    }

    # Set colour for any disbenefits
    benefits$fillcolour[benefits$is_disbenefit] <- fillcolour_disbenefit
    benefits$fontcolour[benefits$is_disbenefit] <- fontcolour_disbenefit

    # Set defaults for intermediates
    xy$y <- NA
    this_head <- rep(0, length(headers))
    benefits$sum <- NA
    benefits$count_next <- 0
    benefits$level_next <- NA
    benefits$order_in_level <- 0
    prev_ihead <- 0
    prev_order <- 0

    # Calculate each y-coordinate
    for (i in 1:nrow(benefits)) {
        ihead <- benefits$LevelID[i]
        if (ihead != prev_ihead) {
            prev_order <- 0
            prev_ihead <- ihead
        }
        prev_order <- prev_order + 1
        benefits$order_in_level[i] <- prev_order
        benefits$sum[i] <- groups$sum[ihead]
        this_head[ihead] <- this_head[ihead] + 1
        xy$y[i] <- maxy - this_head[ihead] / (counts[ihead] + 1) * maxy
        if (ihead == nrow(groups)) next
        benefits$count_next[i] <- counts[ihead + 1]
        benefits$level_next[i] <- headers[ihead + 1]
    }

    # Create nodes used as heading for the levels and apply scale factors
    nodes <-
        DiagrammeR::create_node_df(nheaders,
                                   label = headers_wrapped,
                                   x = 1:nheaders * xscale,
                                   y = maxy * yscale,
                                   shape = "rectangle",
                                   width = 0.9 * xscale,
                                   color = "grey",
                                   height = 0.5 * yscale,
                                   style = "rounded",
                                   fontcolor = "black",
                                   labelloc = "c")

    # Create initial graph
    graph <-  DiagrammeR::create_graph(nodes_df = nodes)

    # Set  up benefits nodes and apply scale factors
    benefit_nodes <- DiagrammeR::create_node_df(nrow(benefits),
                                                x = xy$x * xscale,
                                                y = xy$y * yscale,
                                                width = width * xscale,
                                                fillcolor = benefits$fillcolour,
                                                color = benefits$fillcolour,
                                                shape = "rectangle",
                                                fontcolor = benefits$fontcolour,
                                                fixedsize = FALSE,
                                                label = xy$label)

    # Add benefits to graph
    graph <- DiagrammeR::add_node_df(
        graph,
        node_df = benefit_nodes
    )

    # Create empty dataframe for edges
    edges <- data.frame(from = integer(),
                        linkto = integer(),
                        sum = integer(),
                        count_next = integer()
    )

    # Loop through all LinkTo columns to get edge data
    for (link in links) {
        these_edges <- data.frame(from = 1:nrow(benefits),
                                  linkto = benefits[[link]],
                                  sum = benefits$sum,
                                  limit = benefits$count_next,
                                  level_name = benefits$Level,
                                  element_name = benefits$Element,
                                  next_level = benefits$level_next,
                                  order_in_level = benefits$order_in_level,
                                  is_disbenefit = FALSE)
        these_edges$is_disbenefit[these_edges$linkto < 0] <- TRUE
        edges <- rbind(edges, these_edges)
    }

    # Setup benefit edges
    edges$linkto <- abs(edges$linkto)
    edges$arrowhead <- "open"
    edges$colour <- linkcolour_benefit
    edges$label <- ""
    edges$arrowhead[edges$is_disbenefit] <- "empty"
    edges$colour[edges$is_disbenefit] <- linkcolour_disbenefit
    edges$label[edges$is_disbenefit] <- "-ve"
    edges <- edges[!is.na(edges$linkto),]
    edges$to <- edges$linkto + edges$sum

    # Remove any edges that don't exist
    for (i in 1:nrow(edges)) {
        linkto <- edges$linkto[i]
        if (linkto < 1 | linkto > edges$limit[i])
            warning("Removed invalid connection from ", edges$element_name[i],
                    " (Element ", edges$order_in_level[i], " of ",
                    edges$level_name[i], ") to Element ",
                    edges$linkto[i], " of ", edges$next_level[i])
    }
    edges <- edges[edges$linkto >= 1 & edges$linkto <= edges$limit,]

    benefit_edges <- DiagrammeR::create_edge_df(from = edges$from + nheaders,
                                                to = edges$to + nheaders,
                                                color = edges$colour,
                                                label = edges$label,
                                                arrowhead = edges$arrowhead,
                                                headport = "w",
                                                tailport = "e")

    graph <- DiagrammeR::add_edge_df(
        graph,
        edge_df = benefit_edges
    )

    # Add any measures to the graph
    if ("Measure" %in% colnames(benefits)) {
        mxy <- benefits[!is.na(benefits$Measure),]
        nmeasure <- nrow(mxy)

        if (nmeasure > 0) {
            # Remove quotes from Measures
            mxy$Measure <- gsub("[\"']", "", mxy$Measure)

            # Wrap the text for the elements
            mxy$Measure <-
                unlist(lapply(
                    strwrap(mxy$Measure, width = elements_textwrap, simplify = FALSE),
                    paste,
                    collapse = "\n"
                )
            )

            # Set x-coordinate
            # Get number measures at each level
            mcounts <- unname(sapply(headers, function(h) {
                nrow(mxy[mxy$Level == h,])
            }))
            # Set y-coordinates
            yvals <- xvals <- vector(mode = "numeric", length = 0)
            for (i in mcounts) {
                if (i > 0) {
                    yvals <- c(yvals, seq(i))
                    xvals <- c(xvals, (seq(i) - 1) / max(1, (mcounts - 1)))
                }
            }
            mxy$y <- 1 - yvals
            mxy$x <- mxy$LevelID + (xvals - 0.5) * 0.33

            m_nodes <- DiagrammeR::create_node_df(nrow(mxy),
                                                  x = mxy$x * xscale,
                                                  y = mxy$y * yscale,
                                                  shape = "ellipse",
                                                  width = width * xscale * 0.75,
                                                  fixedsize = FALSE,
                                                  fillcolor = fillcolour_measure,
                                                  fontcolor = fontcolour_measure,
                                                  label = mxy$Measure)
            # Add measures to graph
            graph <- DiagrammeR::add_node_df(
                graph,
                node_df = m_nodes
            )

            # Add extra edges
            start <- max(benefits$NodeID) + 1
            end <- start + nrow(mxy) - 1
            m_edges <- DiagrammeR::create_edge_df(from = mxy$NodeID,
                                                  to = start:end,
                                                  arrowhead = "none",
                                                  tooltip = "Measure",
                                                  style = "dashed")
            graph <- DiagrammeR::add_edge_df(
                graph,
                edge_df = m_edges
            )
        }
    }
    # Set overall font
    graph <- DiagrammeR::add_global_graph_attrs(graph,
                                                "fontname",
                                                fontname,
                                                c("node", "edge"))


    # Add overall title
    if (!is.na(title)) {
        attrs <- c(
            "label",
            "labelloc",
            "fontsize",
            "fontcolor",
            "fontname"
        )
        values <- c(
            title,
            "t",
            14,
            title_colour,
            fontname_title
        )
        attr_types <- c(
            "graph",
            "graph",
            "graph",
            "graph",
            "graph"
        )
        graph <- DiagrammeR::add_global_graph_attrs(graph,
                                                    attr = attrs,
                                                    value = values,
                                                    attr_type = attr_types)
    }

    # Modify edges to shape specified in splines
    graph <- DiagrammeR::add_global_graph_attrs(graph,
                                                "splines",
                                                splines,
                                                "graph")


    # Return graph
    return(graph)

}
