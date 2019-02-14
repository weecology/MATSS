#' @importFrom dplyr %>% pull
#' @importFrom forecast na.interp
#' @importFrom graphics axis mtext par plot points rect text
#' @importFrom lubridate is.Date
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