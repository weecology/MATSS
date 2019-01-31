#' @importFrom dplyr %>% pull
#' @importFrom forecast na.interp
#' @importFrom lubridate is.Date
#' @importFrom stats acf

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