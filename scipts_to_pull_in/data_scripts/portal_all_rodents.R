# devtools::install_github("weecology/portalr")

# dependencies
# library(portalr)
# library(dplyr)

get_data <- function()
{
    raw_counts <- portalr::abundance(shape = "crosstab")
    return(dplyr::select(raw_counts, -period))
}