#' @title Calculate Simpson's diversity index
#'
#' @details There are multiple forms of Simpson's index. Here, we compute
#'   \eqn{1-D}, where \eqn{D_t = sum (p_{t,i})^2}, with \eqn{p_{t,i}} being the
#'   proportional abundance of species \eqn{i} in time index \eqn{t}.
#'
#'   This calculation produces a vector of values, one for each time point.
#'
#' @param dat the dataset
#'
#' @return A vector of the diversity index calculations, one per time point.
#'
#' @examples
#' \dontrun{
#'   sgs_data <- MATSS::get_sgs_data()
#'   compute_simpson_index(sgs_data)
#' }
#'
#' @export
#'
compute_simpson_index <- function(dat)
{
    if (!dat$metadata$is_community)
    {
        message("Skipping...the provided data is not from a community.")
        return(NA)
    }
    
    total <- rowSums(dat$abundance)
    proportions <- sweep(dat$abundance, 1, total, "/")
    proportions_squared <- proportions * proportions
    lambda <- apply(proportions_squared, 1, sum, na.rm = TRUE)
    return(1 - lambda)
}

#' @rdname compute_linear_trend
#' @description `compute_linear_trend_ts()` fits a simple linear model to its
#'   only input, a time series (represented by a numeric vector of values), and
#'   returns the coefficients
compute_linear_trend_ts <- function(ts)
{
    lm_fit <- stats::lm(y ~ t, data = data.frame(y = ts, t = seq_along(ts)))
    stats::coefficients(lm_fit)
}

#' @title Calculate linear trend coefficients
#' @aliases compute_linear_trend_ts
#'
#' @description `compute_linear_trend()` applies `compute_linear_trend_ts()` to
#'   each abundance time series in its input data.
#'
#' @details In the implementation of `compute_linear_trend()`, we use the `MATSS`
#'   function \code{\link[MATSS]{analysis_wrapper}} to construct the version of
#'   `compute_linear_trend_ts()` that can be applied to a whole dataset.
#'
#' @param dataset the dataset
#'
#' @return A tibble, containing the `results`, dataset `metadata`, name of the
#'   input dataset in `dataset`, name of the `method` applied to each time
#'   series (in this case, "compute_linear_trend_ts"), and additional `args`.
#'
#' @examples
#' \dontrun{
#'   sgs_data <- MATSS::get_sgs_data()
#'   compute_linear_trend(sgs_data)
#' }
#'
#' @export
#'
compute_linear_trend <- MATSS::analysis_wrapper(compute_linear_trend_ts)
