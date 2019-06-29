#' @title Macroecological Analayses of Time Series Structure
#'
#' @description Functions for obtaining and processing ecological time series 
#'   datasets, and for running different analyses.
#' 
#' @name MATSS
#'
#' @docType package
#'
#' @keywords package
#'
NULL

.onAttach <- function(libname, pkgname) {
    packageStartupMessage('Please look at our data formats by running `vignette("data-formats")`')
}

#' @importFrom rlang .data

## quiets concerns of R CMD check re: variables used in NSE functions
if (getRversion() >= "2.15.1") utils::globalVariables(
  c(".", "analysis", "combine", "cross", "data", "dataset", "fun", 
    "location_id", "map", "region", "route", "target", "timeperiod_id")
)

#' @title dragons dataset
#'
#' @description A dataset containing example timeseries for some dragons.
#'
#' @format A list with 3 elements:
#' \describe{
#'   \item{abundance}{a data.frame with abundances for 3 dragons}
#'   \item{covariates}{a data.frame with times of observations and effort and precip data}
#'   \item{metadata}{a list with:
#'     `timename` - the name of the time column in covariates,
#'     `period` - the gap between successive observations, 
#'     `authors` - the authors of the dataset, 
#'     `species_table` - information about the species observed}
#' }
"dragons"
