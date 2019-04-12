#' @importFrom graphics axis mtext par plot points rect text
#' @importFrom stats acf cor D median na.omit sd setNames
#' @importFrom utils data read.csv

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

## quiets concerns of R CMD check re: variables used in NSE functions
if (getRversion() >= "2.15.1") utils::globalVariables(
  c("abundance", "analysis", "aou", "bot.t", "censusdate", "code", "combine", "count", "count10", 
    "count20", "count30", "count40", "count50", "countns", "cross", "data", "Date",
    "date_tag", "effort", "fun", "latitude", "longitude", "M", "mutate_all", "n",
    "newmoonnumber", "ntraps", "num_years", "objectid", "period", "quad", 
    "recap", "route", "rpid", "runtype", "seedling", "SESSION", "site_id", 
    "surf.t", "target", "times", "V1", "VEG", "WEB", "x", "y", "Y", "year", "YEAR",
    "species", "species_id", "speciestotal", "spp", "SPP", "statenum", 
    "stems", "Species", "total", "TOTAL", "South", "Central", "North", "FarNorth", 
    "Common.name"
  )
)