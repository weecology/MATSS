#' @title Normalize observations, effort, or times
#' @aliases normalize_effort, normalize_obs
#' 
#' @description \code{normalize_obs} will normalize the observations to the 
#'   provided effort vector, if `obs_per_effort` is TRUE.
#'   
#'   \code{normalize_effort} will generate a default effort vector if it is 
#'   not provided, (effort = 1 over the provided `obs` time series)
#'   
#'   \code{normalize_times} will generate a default times vector if it is 
#'   not provided, (times = `seq(length(obs))`)
#' 
#' @inheritParams ts_summary
#' 
#' @export
normalize_obs <- function(obs, effort, 
                          obs_per_effort = !is.null(effort))
{
    if (!("logical" %in% class(obs_per_effort))) {
        stop("`obs_per_effort` must be logical")
    }
    check_obs(obs)
    if (obs_per_effort) {
        if (NROW(obs) != length(effort)) {
            stop("`obs` and `effort` are not of same length")
        }
        obs <- obs / effort
    }
    return(obs)
}

#' @rdname normalize_obs
#' @export
normalize_times <- function(obs, times = NULL)
{
    if (is.null(times)) {
        message("`time` is `NULL`, assuming evenly spaced data")
        times <- seq_len(NROW(obs))
    }
    check_obs_and_times(obs, times)
    return(times)
}

#' @rdname normalize_obs
#' @export
normalize_effort <- function(obs, effort = NULL)
{
    if (is.null(effort)) {
        message("`effort` is `NULL`, assuming all effort = 1")
        effort <- rep(1, NROW(obs))
    }
    check_effort(effort)
    return(effort)
}

#' @noRd
to_numeric_vector <- function(x)
{
    if (is.data.frame(x))
    {
        x <- as.numeric(x[[1]])
    } else if (is.matrix(x)) {
        x <- as.numeric(x[, 1])
    }
    return(x)
}