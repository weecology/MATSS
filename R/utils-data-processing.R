#' @title Check that the times of a dataset are evenly sampled
#' @aliases is_evenly_sampled
#' 
#' @param data dataset to check
#' @param period period to check the times against (if `NULL`, first check to 
#'   see if there is a known `period` set in the metadata, otherwise assumes 1)
#' @param tol tolerance for the period 
#' 
#' @return TRUE or FALSE, invisibly
#' 
#' @export
is_equitimed <- function(data, period = NULL, tol = 1e-06)
{
    stopifnot(check_data_format(data))
    
    full_times <- get_full_times(data = data, period = period, tol = tol)
    times <- get_times_from_data(data)
    invisible(isTRUE(all.equal(times, full_times)))
}

#' @export
is_evenly_sampled <- is_equitimed

#' @title Insert rows if necessary so that time series are evenly sampled
#' @aliases make_evenly_sampled
#' 
#' @param data dataset to modify
#' @inheritParams is_equitimed
#' @param method one of `c("mean", "method", "closest")` that determines how 
#'   the rows of the original data will get coerced into the output here.
#' @inheritParams base::mean
#' 
#' @return the dataset, with rows coerced according to the equitimed time 
#'   indices, and additional empty rows inserted if needed
#'
#' @details First, `get_full_times()` computes the sequence of time index values 
#'   at a regular sampling interval of period. These will be the final time 
#'   index values for the output. *Some* set of rows of the original dataset 
#'   will map to each of these time indices.
#'   
#'   The `method` argument determines how these rows get coerced:
#'   \describe{
#'     \item{mean}{the values in the rows are averaged together using `mean`}
#'     \item{median}{the values in the rows are averaged together using `median`}
#'     \item{closest}{the values in the row that is closest in time to the 
#'     desired time index are used.}
#'   }
#' 
#' @export
make_equitimed <- function(data, period = NULL, tol = 1e-06, 
                           method = c("mean", "method", "closest"), 
                           na.rm = TRUE)
{
    stopifnot(check_data_format(data))
    
    full_times <- get_full_times(data = data, period = period, tol = tol)
    if (is.null(full_times))
    {
        stop("Unable to construct an evenly spaced time index.")
    }
    
    times <- get_times_from_data(data)
    if (isTRUE(all.equal(times, full_times)))
    {
        message("Dataset is already evenly sampled in time.")
        return(invisible(data))
    }
    
    # generate empty matrices to hold final abundance and covariates
    abundance <- matrix(NA, nrow = length(full_times), ncol = NCOL(data$abundance))
    covariates <- data$covariates[0, , drop = FALSE]

    # compute separation between times and full_times
    times_dist <- outer(times, full_times, function(a, b) {abs(b - a)})
    
    # fill abundance and covariates
    method <- match.arg(method)
    switch(method, 
           mean = {
               idx <- times_dist <= tol
               for (i in seq_along(full_times))
               {
                   abundance[i, ] <- colMeans(data$abundance[idx[, i], , drop = FALSE], na.rm = na.rm)
                   covariates[i, ] <- purrr::map_dfc(data$covariates[idx[, i], , drop = FALSE], mean, na.rm = TRUE)
               }
           }, 
           median = {
               idx <- times_dist <= tol
               for (i in seq_along(full_times))
               {
                   abundance[i, ] <- apply(data$abundance[idx[, i], , drop = FALSE], 2, median, na.rm = na.rm)
                   covariates[i, ] <- purrr::map_dfc(data$covariates[idx[, i], , drop = FALSE], median, na.rm = TRUE)
               }
           }, 
           closest = {
               idx <- apply(times_dist, 2, which.min)
               abundance <- data$abundance[idx,]
               covariates <- data$covariates[idx,]
           })
    
    # restore column names and convert to tibbles
    colnames(abundance) <- colnames(data$abundance)
    abundance <- tibble::as_tibble(abundance)
    covariates <- tibble::as_tibble(covariates)
    
    # make sure times column is properly filled
    time_var <- resolve_covariate_variable(data, "timename")
    if (is.null(time_var))
    {
        # make sure timename variable is unique
        new_col_names <- vctrs::vec_as_names(c(colnames(covariates), "time"), 
                                             repair = "unique", quiet = TRUE)
        time_var <- tail(new_col_names, 1)
        data$metadata$timename <- time_var
    }
    covariates[time_var] <- full_times

    # assemble data to return
    out <- list(abundance = abundance, 
                covariates = covariates, 
                metadata = data$metadata)
    attr(out, "class") <- "matssdata"
    
    return(out)
}

#' @export
make_evenly_sampled <- make_equitimed

#' @title Check if a dataset has integer times
#' 
#' @param data dataset to check
#' 
#' @return TRUE or FALSE
#'
#' @details If the times are already integer or Date, true. Otherwise FALSE, 
#'   with a message if times are missing, or if times could potentially be 
#'   rounded.
#' 
#' @export
has_integer_times <- function(data)
{
    # check for existence of times
    times <- get_times_from_data(data)
    if (is.null(times))
    {
        message("Dataset is missing times.")
        return(FALSE)
    }
    
    # check for integer times
    if (is.integer(times) || inherits(times, "Date"))
    {
        return(TRUE)
    } else if (all(is.wholenumber(times))) {
        message("Dataset has close to integer times, but they need to be rounded.\n", 
                "Perhaps you want to call ", usethis::ui_code("make_integer_times()"), ".\n")
        return(FALSE)
    }
    
    # otherwise
    return(FALSE)
}

#' @title Add a time variable with integer values for evenly sampled data
#' 
#' @param data dataset to modify
#' @inheritParams is_equitimed
#' 
#' @return the dataset, with integer times
#'
#' @details First, check if the data are evenly sampled in time. If not, we 
#'   exit early. Next, if the times are already integer or Date, we don't do 
#'   anything. If the times are numeric, but roundable to integer, we round. 
#'   Otherwise, we add a new variable to `covariates` from 1:n and designate 
#'   this variable as the `timename`.
#' 
#' @export
make_integer_times <- function(data, period = NULL, tol = 1e-06)
{
    times <- get_times_from_data(data)
    
    # do checks based on existing times
    if (!is.null(times))
    {
        # check for equitimed
        if (!is_equitimed(data = data, period = period, tol = tol))
        {
            stop(c("Dataset is not evenly sampled in time.\n",
                   "Perhaps you want to call ", usethis::ui_code("make_equitimed()"), " first.\n"))
        }
        
        # check for integer times
        if (is.integer(times))
        {
            message("Dataset is evenly sampled with integer times already.")
            return(invisible(data))
        } else if (inherits(times, "Date")) {
            message("Dataset is evenly sampled with `Date` formatted times already.")
            return(invisible(data))
        } else if (all(is.wholenumber(times))) {
            message("Dataset is evenly sampled with (close to) integer times already.")
            message("Rounding times to integer and replacing them...")
            time_var <- data$metadata$timename
            data$covariates[time_var] <- as.integer(round(times))
            return(invisible(data))
        }
    }
    
    # add time
    times <- seq_len(NROW(data$abundance))
    if (is.null(data$covariates)) # create covariates
    {
        time_var <- "time"
        data$covariates <- tibble::tibble(time_var = times)
    } else {
        new_col_names <- vctrs::vec_as_names(c(colnames(data$covariates), "time"), 
                                             repair = "unique", quiet = TRUE)
        time_var <- tail(new_col_names, 1)
        data$covariates[time_var] <- times
    }
    data$metadata$timename <- time_var
    message("Integer times created in variable ", usethis::ui_code(time_var), ".")
    return(invisible(data))
}


#' get the complete time index, filling in gaps where necessary, and using the 
#' period to establish the sampling frequency
#' 
#' @noRd
get_full_times <- function(data, period = NULL, tol = 1e-06)
{
    times <- get_times_from_data(data)
    if (is.null(times))
    {
        stop("Dataset does not appear to have a times variable.\n", 
             "Check", usethis::ui_code("covariates"), " and ", 
             usethis::ui_code("metadata$timename"), ".\n")
    }
    period <- get_period_from_data(data, period)
    
    full_times <- tryCatch(tidyr::full_seq(times, period, tol), 
                           error = function(e) {
                               message(e$message)
                               return(NULL)
                           })
    return(full_times)
}

#' extract the period, given the value from the metadata field, and a value 
#' specified by the user. The flowchart is:
#'   (1) if user has supplied non-null `period`, use that
#'   (2) if metadata period is non-null, use that
#'   (3) use a default value of 1 and print a message
#' 
#' @noRd
get_period_from_data <- function(data, period = NULL)
{
    if (is.null(period))
    {
        period <- data$metadata$period
        if (is.null(period))
        {
            message("No time period found. Assuming period = 1.")
            period <- 1
        }
    }
    return(period)
}

#' @noRd
is.wholenumber <- function(x, tol = .Machine$double.eps ^ 0.5)
{
    abs(x - round(x)) < tol
}


