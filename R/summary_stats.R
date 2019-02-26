#' @title Summarize a time series
#'
#' @description \code{ts_summary}: summarize a potentially multivariate set of
#'   time series observations. \cr \cr
#'   \code{uni_ts_summary}: summarize a univariate set of time series 
#'   observations.
#'   Summaries occur based on \code{times}, with effort-based correction (if
#'   requested via \code{obs_per_effort}) and interpretation of missing 
#'   values for autocorrelation calcuations (if needed) via 
#'   \code{interp_method}. \cr \cr
#'   \code{ts_summary_drake}: operates \code{ts_summary} but on a \code{list} 
#'   data structure used in the MATSS pipeline with \code{abundance}, 
#'   \code{covariates} and \code{metadata} elements.
#'
#' @param obs \code{ts_summary}: a 1- or 2-dimensional set of \code{numeric}
#'   observations (columns if 2-dimensional) across times (rows if 
#'   2-dimensional) as indexed by \code{times}. \cr \cr
#'   \code{uni_ts_summary}: a 1-dimensional \code{numeric} set of observations
#'   across times as indexed by \code{times}.   
#'
#' @param times \code{numeric} or \code{Date} vector of timestamps of the 
#'   observations.
#'
#' @param effort \code{numeric} vector of effort associated with the 
#'   observations.
#'
#' @param obs_per_effort \code{logical} indicator if \code{obs} should be 
#'   corrected for \code{effort} before summaries are done.
#'
#' @param interp_method \code{character} representing a function name used to 
#'   interpolate \code{obs}. Defaults to \code{"\link[forecast]{na.interp}"}.
#'
#' @param x Three-element \code{list} (\code{abundance}, \code{covariates},
#'   \code{metadata}) for data objects in the MATSS pipeline. 
#'
#' @return \code{ts_summary}: \code{list} of number of species, number of
#'   observations, species richness summary, total observation summary, 
#'   among-species correlation, species-by-species summary, times summary, 
#'   and effort summary.
#'   \cr \cr
#'   \code{uni_ts_summary}: \code{list} of observation summary, times summary,
#'   effort summary, and autocorrelation.
#'   \cr \cr
#'   \code{ts_summary_drake}: \code{list} as with \code{ts_summary} but based
#'   on the \code{list} data structure used in the MATSS pipeline with 
#'   \code{abundance}, \code{covariates} and \code{metadata} elements.
#'
#' @export
#'
ts_summary <- function(obs, times = NULL, effort = NULL, 
                       obs_per_effort = FALSE, 
                       interp_method = "na.interp") {
    if (length(interp_method) > 1 || !is.character(interp_method)) {
        stop("`interp_method` is not a single character input")
    }
    if (is.null(times)) {
        message("`time` is `NULL`, assuming evenly spaced data")
        times <- seq_len(NROW(obs))
    }
    check_obs_and_times(obs, times, single_dim_obs = FALSE)
    obs <- data.frame(obs)
    if (!("logical" %in% class(obs_per_effort))) {
        stop("`obs_per_effort` must be logical")
    }    
    if (obs_per_effort) {
        if (is.null(effort)) {
            message("`effort` is `NULL`, assuming all effort = 1")
            effort <- rep(1, length(obs))
            effort_summary <- NULL
        }
        if (nrow(obs) != length(effort)) {
            stop("`obs` and `effort` are not of same length")
        }
        obs <- obs / effort
    } else if (!is.null(effort)) {
        warning("`effort` is included but `obs_per_effort` is FALSE, `obs` not
            corrected for effort")
    }
    
    nspp <- ncol(obs)  
    nobs <- nrow(obs)
    spp_richness <- apply(obs, 1, richness)
    spp_richness_summary <- uni_ts_summary(spp_richness, times, 
                                           interp_method = interp_method)
    
    tot_obs <- apply(obs, 1, sum, na.rm = TRUE)
    tot_obs_summary <- uni_ts_summary(tot_obs, times, 
                                      interp_method = interp_method)
    
    sp_level_summaries <- vector("list", length = nspp)
    for (i in 1:nspp) {
        sp_level_summaries[[i]] <- uni_ts_summary(obs[ ,i], times, 
                                                  interp_method = interp_method)
    }
    names(sp_level_summaries) <- colnames(obs)
    
    effort_summary <- summarize_effort(obs, effort)
    times_summary <- summarize_times(obs, times)
    
    list(n_spp = nspp,
         n_obs = nobs,
         spp_richness = spp_richness_summary,
         total_obs = tot_obs_summary,
         among_spp_correlations = round(cor(obs), 3),
         species_obs = sp_level_summaries,
         times = times_summary,
         effort = effort_summary)
}

#' @rdname ts_summary
#'
#' @export
#'
uni_ts_summary <- function(obs, times = NULL, effort = NULL, 
                           obs_per_effort = FALSE, 
                           interp_method = "na.interp") {
    if (length(interp_method) > 1 || !is.character(interp_method)) {
        stop("`interp_method` is not a single character input")
    }
    if (is.null(times)) {
        message("`time` is `NULL`, assuming evenly spaced data")
        times <- seq_len(NROW(obs))
    }
    check_obs_and_times(obs, times)
    
    if (!("logical" %in% class(obs_per_effort))) {
        stop("`obs_per_effort` must be logical")
    }  
    if (obs_per_effort) {
        if (is.null(effort)) {
            message("`effort` is `NULL`, assuming all effort = 1")
            effort <- rep(1, length(obs))
        }
        if(length(obs) != length(effort)) {
            stop("`obs` and `effort` are not of same length")
        }
        obs <- obs / effort
    } else if (!is.null(effort)) {
        warning("`effort` is included but `obs_per_effort` is FALSE, `obs` not
            corrected for effort")
    }
    
    obs_summary <- summarize_obs(obs)
    times_summary <- summarize_times(obs, times)
    effort_summary <- summarize_effort(obs, effort)
    auto_cor <- temp_autocor(obs, times, interp_method)
    list(observations = obs_summary, 
         times = times_summary, 
         effort = effort_summary,
         autocorrelation = auto_cor)
}

#' @rdname ts_summary
#'
#' @export
#'
ts_summary_drake <- function(x) {
    if (!is.null(x$metadata$times)) {
        times <- pull(x$covariates, x$metadata$times)
    } else{
        times <- NULL
    }  
    if (!is.null(x$metadata$effort)) {
        effort <- pull(x$covariates, x$metadata$effort)
    } else{
        effort <- NULL
    }
    ts_summary(x$abundance, times = times, effort = effort)
}

#' @title Summarize univariate observations, times, or efforts
#'
#' @param obs \code{numeric} vector of observations.
#'
#' @param round_out \code{logical} indicator if rounding should happen.
#'
#' @param digits \code{NULL} (default) or \code{integer} value of the number
#'   of digits for rounding. If \code{NULL}, \code{digits} is calculated to
#'   be two order of magnitude lower than the smallest value in the vector
#'   being summarized. 
#'
#' @return \code{list} with entries corresponding to the mininum, maximum,
#'   median, mean, standard deviation, and count of the observations, times,
#'   or effort, rounded based on \code{round_out} and \code{digits}.
#' 
#' @export
#'
summarize_obs <- function(obs, round_out = TRUE, digits = NULL) {
    check_obs(obs)
    if (!("logical" %in% class(round_out))) {
        stop("`round_out` must be logical")
    }
    obs <- na.omit(as.numeric(obs))
    out <- c(min = min(obs), max = max(obs), median = median(obs), 
             mean = mean(obs), sd = sd(obs), n = length(obs))
    if (round_out) {
        if (is.null(digits)) {
            digits <- max(c(0, 2 + -floor(log10(min(obs[obs > 0])))))
        } else if (digits %% 1 != 0) {
            stop("`digits` must be an integer")
        }
        out <- round(out, digits)
    }
    out
}

#' @rdname summarize_obs
#'
#' @export
summarise_obs <- summarize_obs

#' @rdname summarize_obs
#'
#' @param times \code{numeric} or \code{Date} vector of timestamps of the 
#'   observations.
#'  
#' @export
#'
summarize_times <- function(obs, times, round_out = TRUE, digits = NULL) {
    check_obs_and_times(obs, times)
    
    if (!("logical" %in% class(round_out))) {
        stop("`round_out` must be logical")
    }
    obs <- data.frame(obs)
    obs2 <- is.na(obs)
    allna <- apply(obs2, 1, sum) == ncol(obs)
    times <- times[!allna]
    out <- c(min = min(times), max = max(times), median = median(times), 
             mean = mean(times), sd = sd(times), n = length(times))
    if (round_out) {
        if (is.null(digits)) {
            digits <- max(c(1, 2 + -floor(log10(min(times[times > 0])))))
        } else if (digits %% 1 != 0) {
            stop("`digits` must be an integer")
        }
        out <- round(out, digits)
    }
    out
}

#' @rdname summarize_obs
#'
#' @export
summarise_times <- summarize_times

#' @rdname summarize_obs
#'
#' @param effort \code{numeric} vector of effort associated with the 
#'   observations.
#'
#' @export
#'
summarize_effort <- function(obs, effort, round_out = TRUE, digits = NULL) {
    if (is.null(effort)) {
        message("`effort` is `NULL`, assuming all effort = 1")
        effort <- rep(1, NROW(obs))
    }
    check_effort(effort)
    check_obs(obs, single_dim_obs = FALSE)
    
    if (!("logical" %in% class(round_out))) {
        stop("`round_out` must be logical")
    }
    obs <- data.frame(obs)
    obs2 <- is.na(obs)
    allna <- apply(obs2, 1, sum) == ncol(obs)
    effort <- effort[!allna]
    out <- c(min = min(effort), max = max(effort), median = median(effort), 
             mean = mean(effort), sd = sd(effort), n = length(effort))
    if (round_out) {
        if (is.null(digits)) {
            digits <- max(c(1, 2 + -floor(log10(min(effort[effort > 0])))))
        } else if (digits %% 1 != 0) {
            stop("`digits` must be an integer")
        }
        out <- round(out, digits)
    }
    out
}

#' @rdname summarize_obs
#'
#' @export
summarise_effort <- summarize_effort

#' @title Count non-0 entries
#'
#' @description Calculate the richness (number of non-0 entries) of a given 
#'   sample.
#'
#' @param x \code{numeric} vector
#'
#' @return \code{numeric} value of the number of non-0 entries in \code{x}.
#'
#' @export
#'
richness <- function(x) {
    if (!is.numeric(x)) {
        stop("`x` must be numeric")
    }
    if (!is.vector(x)) {
        stop("`x` must be a vector")
    }
    x <- na.omit(x)
    length(x[x > 0])
}


#' @title Interpolate observations
#'
#' @description Calculate the autocorrelation of the observations based on 
#'   their timestamps, with interpolation (based on a specified method) if 
#'   necessary.
#'
#' @inheritParams ts_summary
#' @param ... further arguments to be passed to acf
#'
#' @return Autocorrelation of the observation vector.
#'
#' @export
#'
temp_autocor <- function(obs, times, interp_method = "na.interp", ...) {
    check_obs_and_times(obs, times)

    if (length(interp_method) > 1 || !is.character(interp_method)) {
        stop("`interp_method` is not a single character input")
    }
    obs_interp <- interpolate_obs(obs, times, interp_method)
    ac <- acf(obs_interp, plot = FALSE, ...)
    out <- round(ac$acf[ , , 1]  , 4)
    names(out) <- ac$lag[ , , 1]
    out
}

#' @title Interpolate observations
#'
#' @description Interpolate observations based on their timestamps and a 
#'   method.
#' 
#' @inheritParams ts_summary
#' @param ... further arguments to be passed to the interpolation method
#'
#' @return Interpolated observation vector.
#'
#' @export
#'
interpolate_obs <- function(obs, times, interp_method = "na.interp", ...) {
    check_obs_and_times(obs, times)
    
    if (length(interp_method) > 1 || !is.character(interp_method)) {
        stop("`interp_method` is not a single character input")
    }
    time_diff <- diff(times)
    times_interp <- min(times):max(times)
    ntimes <- length(times_interp)
    nspp <- ncol(data)
    out <- rep(NA, ntimes)
    colnames(out) <- colnames(data)
    for (i in 1:ntimes) {
        time_match <- which(times_interp == times[i])
        out[time_match] <- obs[i]
    }
    do.call(interp_method, list(out), ...)
}

#' @title Check if `obs` and `times` is properly formatted
#'
#' @noRd
check_obs_and_times <- function(obs, times, single_dim_obs = TRUE)
{
    check_obs(obs, single_dim_obs = single_dim_obs)
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
check_obs <- function(obs, single_dim_obs = TRUE)
{
    if (!is.numeric(obs)) {
        stop("`obs` must be numeric")
    }
    if (single_dim_obs && !is.null(dim(obs))) {
        stop("`obs` must be a single dimension")
    }
}

#' @title Check if `times` is properly formatted
#'
#' @noRd
check_times <- function(times)
{
    if (!is.numeric(times)) {
        if (!is.Date(times)) {
            stop("`times` must be numeric or a Date")
        }
        times <- as.numeric(difftime(times, min(times), units = "days")) + 1
    }
    if (!is.null(dim(times))) {
        stop("`times` must be a single dimension")
    }
}
