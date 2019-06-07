#' @title Summarize a time series
#'
#' @description \code{ts_summary} creates a summary of a community time series 
#'   dataset. The summary contains community-level statistics, including total 
#'   number of observations, species richness, cross-correlations; as well as 
#'   summary statistics on the individual populations that make up the 
#'   community.
#' 
#'   \code{ts_summary_drake} is the same as \code{ts_summary}, but operates on 
#'   the default data structure used in MATSS, as described in 
#' 
#'   Some aspects of the summaries depend on \code{times}, which should be a 
#'   vector of the time index associated with the time series; and 
#'   \code{effort}, which should be a vector of the sampling effort. 
#'   \code{obs_per_effort} is an optional argument for correcting abundance 
#'   based on effort; by default, it checks if \code{effort} is NULL.
#'   Interpolation of missing values for autocorrelation calcuations (if 
#'   needed) is done via \code{interp_method}
#'   
#' @param obs a vector, matrix, or data.frame of \code{numeric} observations 
#'   (within columns) across times (within rows). 
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
#' @param interp_method \code{character} a function used to interpolate 
#'   \code{obs}. Defaults to \code{\link[forecast]{na.interp}}.
#'
#' @return \code{ts_summary}: \code{list} of number of species, number of
#'   observations, summaries of the variables, the times, the effort, the 
#'   species richness, total observation, and the among-species correlation.
#'   \cr \cr
#'   \code{ts_summary_drake}: \code{list} as with \code{ts_summary} but based
#'   on the \code{list} data structure used in the MATSS pipeline with 
#'   \code{abundance}, \code{covariates} and \code{metadata} elements.
#'
#' @export
#'
ts_summary <- function(obs, times = NULL, effort = NULL, 
                       obs_per_effort = !is.null(effort), 
                       interp_method = forecast::na.interp, 
                       ...)
{
    # normalize obs, times, effort
    times <- normalize_times(obs, times)
    effort <- normalize_effort(obs, effort)
    obs <- normalize_obs(obs, effort, obs_per_effort)
    
    # compute community properties
    num_spp <- NCOL(obs)
    num_obs <- NROW(obs)
    if (is.null(dim(obs))) # NOT a data.frame or matrix
    {
        obs <- data.frame(obs)
    }
    spp_richness <- apply(obs, 1, richness)
    tot_obs <- apply(obs, 1, sum, na.rm = TRUE)
    
    # assemble data.frame of variables
    df <- data.frame(obs, 
                     times = times, 
                     effort = effort, 
                     richness = spp_richness, 
                     tot_obs = tot_obs)
    
    # compute summaries and assemble output
    list(num_spp = num_spp,
         num_obs = num_obs,
         stats = summarize_df(df, times, interp_method, ...), 
         spp_correlations = round(stats::cor(obs), 4))
}


#' @rdname ts_summary
#'
#' @export
#'
ts_summary_drake <- function(x)
{
    time_var <- x$metadata$timename
    if (!is.null(time_var)) {
        times <- dplyr::pull(x$covariates, time_var)
    } else {
        times <- NULL
    }
    
    effort_var <- x$metadata$effort
    if (!is.null(effort_var)) {
        effort <- dplyr::pull(x$covariates, effort_var)
    } else{
        effort <- NULL
    }
    ts_summary(x$abundance, times = times, effort = effort)
}

#' @title Compute summaries and autocorrelation for each variable
#' @aliases summarise_df
#' 
#' @param df the data.frame of variables to summarize
#' @param times the time indices associated with the rows of `df`
#' @inheritParams temp_autocor
#'
#' @export
summarize_df <- function(df, times = seq_len(NROW(df)), 
                         interp_method = forecast::na.interp, ...)
{
    autocorr <- function(v) { temp_autocor(v, times, interp_method, ...) }
    df %>%
        purrr::map_dfr(summarize_vec, .id = "variable") %>%
        dplyr::mutate(autocorrelation = purrr::map(df, autocorr)) %>%
        tibble::as_tibble()
}
#' @rdname summarize_df
#' @export
summarise_df <- summarize_df

#' @title Summarize a univariate vector
#' @aliases summarise_vec
#' 
#' @param x the vector to be summarized
#'
#' @param round_out \code{logical} indicator if rounding should happen.
#'
#' @param digits \code{NULL} (default) or \code{integer} value of the number
#'   of digits for rounding. If \code{NULL}, \code{digits} is calculated to
#'   be two order of magnitude lower than the smallest value in the vector
#'   being summarized. 
#'
#' @return \code{vector} with entries corresponding to the mininum, maximum,
#'   median, mean, standard deviation, and count of the observations, times,
#'   or effort, rounded based on \code{round_out} and \code{digits}.
#' 
#' @export
summarize_vec <- function(x, round_out = TRUE, digits = NULL)
{
    if (!("logical" %in% class(round_out))) {
        stop("`round_out` must be logical")
    }
    x <- na.omit(to_numeric_vector(x))
    out <- data.frame(min = min(x), max = max(x), median = stats::median(x), 
                      mean = mean(x), sd = stats::sd(x), n = length(x))
    if (round_out) {
        if (is.null(digits)) {
            digits <- max(c(1, 2 + -floor(log10(min(x[x > 0])))))
        } else if (digits %% 1 != 0) {
            stop("`digits` must be an integer")
        }
        out <- round(out, digits)
    }
    out
}
#' @rdname summarize_vec
#' @export
summarise_vec <- summarize_vec

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
richness <- function(x)
{
    if (!is.numeric(x)) {
        stop("`x` must be numeric")
    }
    if (!is.vector(x)) {
        stop("`x` must be a vector")
    }
    x <- stats::na.omit(x)
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
temp_autocor <- function(obs, times, interp_method = forecast::na.interp, ...)
{
    obs <- to_numeric_vector(obs)
    obs_interp <- interpolate_obs(obs, times, interp_method)
    ac <- stats::acf(obs_interp, plot = FALSE, ...)
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
interpolate_obs <- function(obs, times, interp_method = forecast::na.interp, ...)
{
    check_interp_method(interp_method)
    
    # get subset of observations at the value of times to be interpolated
    times_interp <- seq(from = min(times), to = max(times))
    out <- obs[match(times_interp, times)]
    
    # set column names of the output
    colnames(out) <- colnames(obs)
    
    # interpolate and return
    interp_method(out, ...)
}
