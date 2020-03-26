#' @title Summarize a time series dataset
#'
#' @param x Class `matssdata` object to be summarized.
#' @param ... additional arguments to `ts_summary()`
#' 
#' @return \code{list} of number of species, number of
#'   observations, summaries of the variables, the times, the effort, the 
#'   species richness, total observation, and the among-species correlation.
#' 
#' @export
summary.matssdata <- ts_summary

# print the variables of a tibble or data.frame
print_tbl_info <- function(tbl, tbl_name = "Abundances")
{
    nrows <- NROW(tbl)
    ncols <- NCOL(tbl)
    comment <- paste0("# ", tbl_name, ": ", nrows, " x ", ncols)
    cat(pillar::style_subtle(comment), "\n")
    cat(pillar::style_subtle("#   variables: "), "\n")
    print(names(tbl))
}

# print a horizontal line
print_hrule <- function(width = getOption("width"))
{
    line_text <- paste0(c(rep.int("-", width), "\n"), collapse = "")
    cat(pillar::style_subtle(line_text))
}

#' @title Print a time series dataset
#'
#' @param x Class `matssdata` object to be printed
#' @param ... additional arguments (unused)
#' 
#' @export
print.matssdata <- function(x, ...)
{
    print_hrule()
    print_tbl_info(x$abundance, "Abundances")

    print_hrule()
    print_tbl_info(x$covariates, "Covariates")
    
    print_hrule()
    comment <- paste0("# Metadata: ", length(x$metadata), " fields")
    cat(pillar::style_subtle(comment), "\n")
    str(x$metadata, no.list = TRUE, max.level = 1)
    invisible(x)
}
