#' @title Check that the times of a dataset are evenly sampled
#' @aliases is_evenly_sampled
#' 
#' @param data dataset to check
#' @param period period to check the times against (if `NULL`, first check to 
#'   see if there is a known `period` set in the metadata, otherwise assumes 1)
#' @param tol tolerance for the period 
#' @param return_if_time_missing default value to return if times are missing
#' 
#' @return TRUE or FALSE, invisibly
#' 
#' @export
is_equitimed <- function(data, period = NULL, tol = 1e-06, 
                         return_if_time_missing = TRUE)
{
    stopifnot(check_data_format(data))
    
    times <- get_times_from_data(data)
    if (is.null(times))
    {
        message("No time index found. Returning ", return_if_time_missing)
        return(return_if_time_missing)
    }
    period <- get_period(data$metadata$period, period)

    full_times <- tryCatch(tidyr::full_seq(times, period, tol), 
                           error = function(e) {
                               message(e)
                               return(NULL)
                           })
    invisible(isTRUE(all.equal(times, full_times)))
}

#' @export
is_evenly_sampled <- is_equitimed

#' extract the period, given the value from the metadata field, and a value 
#' specified by the user. The flowchart is:
#'   (1) if user has supplied non-null `period`, use that
#'   (2) if metadata period is non-null, use that
#'   (3) use a default value of 1 and print a message
#' 
#' @noRd
get_period <- function(metadata_period, period = NULL)
{
    if (is.null(period))
    {
        period <- metadata_period
        if (is.null(period))
        {
            message("No time period found. Assuming period = 1.")
            period <- 1
        }
    }
    return(period)
}





