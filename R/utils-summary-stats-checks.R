#' @title Check if `interp_method` is properly formatted
#' 
#' @noRd
check_interp_method <- function(interp_method)
{
    if (length(interp_method) > 1 || !is.function(interp_method)) {
        stop("`interp_method` is not a single function input")
    }
}

#' @title Check if `obs` and `times` is properly formatted
#'
#' @noRd
check_obs_and_times <- function(obs, times)
{
    check_obs(obs)
    check_times(times)
    if (length(times) != NROW(obs)) {
        stop("`obs` and `times` are not of same length")
    }
}

#' @title Check if `effort` is properly formatted
#'
#' @noRd
check_effort <- function(effort)
{
    if (!is.numeric(effort)) {
        stop("`effort` must be numeric")
    }
    if (!is.null(dim(effort))) {
        stop("`effort` must be a single dimension")
    }
}

#' @title Check if `obs` is properly formatted
#'
#' @noRd
check_obs <- function(obs)
{
    if (is.data.frame(obs))
    {
        if (!all(vapply(obs, is.numeric, TRUE)))
        {
            stop("`obs` must have only numeric columns")
        }
    } else if (!is.numeric(obs)) {
        stop("`obs` must be numeric")
    }
}

#' @title Check if `times` is properly formatted
#'
#' @noRd
check_times <- function(times)
{
    if (!is.numeric(times)) {
        if (!lubridate::is.Date(times)) {
            stop("`times` must be numeric or a Date")
        }
    }
    if (!is.null(dim(times))) {
        stop("`times` must be a single dimension")
    }
}
