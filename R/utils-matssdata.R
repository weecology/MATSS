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
summary.matssdata <- function(x, ...)
{
    stopifnot(check_data_format(x))
    ts_summary(x, ..., include_spp_correlations = FALSE)
}

#' @title Print a time series summary
#'
#' @param x Class `matsssummary` object to be printed
#' @param ... additional arguments (unused)
#' 
#' @export
print.matsssummary <- function(x, ..., n = NULL)
{
    cat(pillar::style_subtle(paste0("# Abundance matrix: ", x$num_obs, " obs x ", x$num_spp, " spp")), 
        "\n")
    
    details <- tibble::trunc_mat(x$stats[[1]], n = n)
    cat(format(details)[-1], sep = "\n")
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
    print_info(x$abundance, "$abundance")
    
    print_hrule()
    print_info(x$covariates, "$covariates")
    
    print_hrule()
    print_info(x$metadata, "$metadata")

    invisible(x)
}


# print the info about a tibble or list
print_info <- function(x, name = "$abundance", details = TRUE, width = 13)
{
    header <- format_info_header(x)
    cat(format(name, width = width), header, "\n")
    print_details(x, details)
}

# format header info for an object
format_info_header <- function(x)
{
    if (inherits(x, "tbl"))
    {
        pillar::style_subtle(paste0("# A tibble: ", NROW(x), " x ", NCOL(x)))
    } else if (inherits(x, "data.frame")) {
        pillar::style_subtle(paste0("# A data.frame: ", NROW(x), " x ", NCOL(x)))
    } else if (inherits(x, "list")) {
        pillar::style_subtle(paste0("# List of ", length(x)))
    }
}

# print the details of an object
print_details <- function(x, details = TRUE)
{
    if (!details) return()
    
    if (inherits(x, "data.frame"))
    {
        cat(pillar::style_subtle("#   variables: "), "\n")
        print(names(x))
    } else if (inherits(x, "list")) {
        str(x, no.list = TRUE, max.level = 1, indent.str = "  ..")
    }
}

# print a horizontal line
print_hrule <- function(width = getOption("width"))
{
    line_text <- paste0(c(rep.int("-", width), "\n"), collapse = "")
    cat(pillar::style_subtle(line_text))
}
