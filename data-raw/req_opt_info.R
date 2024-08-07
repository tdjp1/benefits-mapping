#
# Define the required (required_info) and optional columns for BenefitsMapping
#
required_info <- c("Level",
                   "Element",
                   "LeadsTo1")

optional_info <- c("Measure")

benefits_fill_colours <- c("#cf8063","#eeaf92","#fde6db","#dba89e","#f2b6b0")
benefits_text_colours <- rep("#000000", 5)
disbenefits_fill_colour <- c("5d576b")
disbenefits_text_colour <- c("#ffffff")
measure_fill_colour <- c("#999999")
measure_text_colour <- c("#FFFFFF")

usethis::use_data(required_info,
                  optional_info,
                  benefits_fill_colours,
                  benefits_text_colours,
                  disbenefits_fill_colour,
                  disbenefits_text_colour,
                  measure_fill_colour,
                  measure_text_colour,
                  internal = TRUE,
                  overwrite = TRUE)
