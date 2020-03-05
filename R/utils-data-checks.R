#' @title Check data format
#'
#' @description Check whether its input matches the specified data format in 
#'   the `data-formats.Rmd` vignette
#'
#' @param data dataset to check
#'
#' @return TRUE or FALSE
#' 
#' @export

check_data_format <- function(data)
{
    # check if top-level is a list
    if (!is.list(data))
    {
        message("The provided data was not a list.")
        return(FALSE)
    }
    
    # check if top-level is a data.frame
    if (is.data.frame(data))
    {
        message("The provided data was a data.frame and not a list.")
        return(FALSE)
    }
    
    check_result <- check_abundance(data) && 
        check_covariates(data) && 
        check_metadata(data)
    
    return(check_result)
}

#' @noRd
check_abundance <- function(data)
{
    if (!("abundance" %in% names(data)))
    {
        message("Data did not have an `abundance` element.")
        return(FALSE)
    }
    
    # check that abundance is a data.frame
    if (!is.data.frame(data$abundance))
    {
        message("`abundance` is not a data.frame.")
        return(FALSE)
    }
    
    # check that abundance has only numeric columns
    if (!(all(vapply(data$abundance, class, "") %in% 
              c("numeric", "integer"))))
    {
        message("Some columns in `abundance` were not numeric or integer.")
        return(FALSE)
    }
    
    # check if top-level has covariates
    if (!check_covariates(data))
        return(FALSE)
    
    # check if top-level has metadata
    if (!check_metadata(data))
        return(FALSE)

    # else

    return(TRUE)
}

#' @noRd
check_metadata <- function(data)
{
    if (!("metadata" %in% names(data)))
    {
        message("Data did not have a `metadata` element.")
        return(FALSE)
    }
    
    # check that data$metadata is a list
    if (!is.list(data$metadata))
    {
        message("`metadata` is not a list.")
        return(FALSE)
    }
    
    check_result <- check_metadata_time(data) && 
        check_metadata_effort(data) && 
        check_metadata_species_table(data) && 
        check_metadata_community_flag(data) && 
        check_metadata_citation(data) && 
        check_metadata_location(data)
    
    return(check_result)
}

#' @noRd
check_metadata_time <- function(data)
{
    # resolve timename and check
    time_var <- resolve_covariate_variable(data, "timename")
    if (!is.null(time_var))
    {
        times <- dplyr::pull(data$covariates, time_var)
        
        times <- as.numeric(times)
        if (any(is.na(times)))
        {
            message("`times` could not be coerced into numeric.")
            return(FALSE)
        }
    }
    return(TRUE)
}

#' @noRd
check_metadata_effort <- function(data)
{
    # resolve effort and check
    effort_var <- resolve_covariate_variable(data, "effort")
    if (!is.null(effort_var))
    {
        effort <- dplyr::pull(data$covariates, effort_var)
        if (!is.numeric(effort))
        {
            message("`metadata$effort` is not numeric.")
            return(FALSE)
        }
        if (any(is.na(effort)) || any(effort <= 0))
        {
            message("`metadata$effort` contains invalid values (NA or non-positive).")
            return(FALSE)
        }
    }
    return(TRUE)
}

#' @noRd
check_metadata_species_table <- function(data)
{
    if ("species_table" %in% names(data$metadata))
    {
        species_table <- data$metadata$species_table
        if (!is.data.frame(species_table))
        {
            message("`metadata$species_table` is not a data.frame.")
            return(FALSE)
        }
        if (!("id" %in% names(species_table)))
        {
            message("`metadata$species_table` does not have an `id` variable.")
            return(FALSE)
        }
        if (!all(names(data$abundance) %in% as.character(species_table$id)))
        {
            message("`abundance` has columns not listed in `metadata$species_table`")
            return(FALSE)
        }
    }
    return(TRUE)
}

#' @noRd

check_metadata_community_flag <- function(data)
{
    if ("is_community" %in% names(data$metadata))
    {
        if (!is.logical(data$metadata$is_community))
        {
            message("`metadata$is_community` is not logical.")
            return(FALSE)
        }
        if (length(data$metadata$is_community) < 1)
        {
            message("`metadata$is_community` has zero length.")
            return(FALSE)
        }
        return(TRUE)
    }
    message("`metadata$is_community` is missing.")
    return(FALSE)
}

check_metadata_citation <- function(data)
{
    if (!"citation" %in% names(data$metadata))
    {
        message("The provided data did not have an `metadata$citation` element.")
        return(FALSE)
    }
    
    if (!"character" %in% class(data$metadata$citation))
    {
        message("`data$metadata$citation` is not a character vector.")
        return(FALSE)
    }
    
    if (length(data$metadata$citation) < 1)
    {
        message("`data$metadata$citation` has no entries.")
        return(FALSE)
    }
    
    return(TRUE)
}

#' @noRd
check_metadata_location <- function(data)
{
    if ("location" %in% names(data$metadata))
    {
        if (!all(c("latitude", "longitude") %in% names(data$metadata$location)))
        {
            message("`metadata$location` is missing `latitude` and/or `longitude`.")
            return(FALSE)
        }
        
        latitude <- data$metadata$location[["latitude"]]
        if (!class(latitude) %in% c("numeric", "integer"))
        {
            message("`metadata$location$latitude` is not numeric.")
            return(FALSE)
        }
        if (length(latitude) < 1)
        {
            message("`metadata$location$latitude` has no entries.")
            return(FALSE)
        }
        
        longitude <- data$metadata$location[["longitude"]]
        if (!class(longitude) %in% c("numeric", "integer"))
        {
            message("`metadata$location$longitude` is not numeric.")
            return(FALSE)
        }
        if (length(longitude) < 1)
        {
            message("`metadata$location$longitude` has no entries.")
            return(FALSE)
        }
    }
    return(TRUE)
}

#' @noRd
check_covariates <- function(data)
{
    if ("covariates" %in% names(data))
    {
        # check that data$covariates is a data.frame
        if (!is.data.frame(data$covariates))
        {
            message("`covariates` is not a data.frame.")
            return(FALSE)
        }
        
        # check that data$covariates has the correct number of rows
        if (NROW(data$covariates) != NROW(data$abundance))
        {
            message("`covariates` has a different number of rows than `abundance`.")
            return(FALSE)
        }
    }
    return(TRUE)
}

#' @title extract the times from a formatted data structure
#' 
#' @param data a formatted data structure
#' 
#' @return a \code{numeric} or \code{Date} vector containing the times
#' 
#' @export
get_times_from_data <- function(data)
{
    get_covariate_from_data(data, "timename")
}

#' @title extract the effort from a formatted data structure
#' 
#' @param data a formatted data structure
#' 
#' @return a \code{numeric} vector containing the effort
#' 
#' @export
get_effort_from_data <- function(data)
{
    get_covariate_from_data(data, "effort")
}

#' @noRd
resolve_covariate_variable <- function(data, covariate_id)
{
    if ("metadata" %in% names(data) &&                       # metadata exists
        covariate_id %in% names(data$metadata) &&            # covariate_id exists in metadata
        "covariates" %in% names(data) &&                     # covariates exists
        !is.null(data$metadata[[covariate_id]][1]) &&        # var_name resolves
        data$metadata[[covariate_id]][1] %in% names(data$covariates)) # var_name exists in covariates
    {
        return(data$metadata[[covariate_id]][1])
    }
    # else
    return(NULL)
}

#' @noRd
get_covariate_from_data <- function(data, covariate_id)
{
    var_name <- resolve_covariate_variable(data, covariate_id)
    if (!is.null(var_name))
    {
        covariate <- dplyr::pull(data$covariates, var_name)
    } else {
        covariate <- NULL
    }
    return(covariate)
}

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
