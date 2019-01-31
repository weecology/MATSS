#' @title Check data format
#'
#' @description Check whether its input matches the specified data format in 
#'   the `data-formats.Rmd` vignette
#'
#' @param data
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
    
    # check if top-level has abundance
    if (!("abundance" %in% names(data)))
    {
        message("The provided data did not have an `abundance` element.")
        return(FALSE)
    }
    
    # check that abundance is a data.frame
    if (!is.data.frame(data$abundance))
    {
        message("data$abundance is not a data.frame.")
        return(FALSE)
    }
    
    # check that abundance  has only numeric columns
    if (!(all(vapply(data$abundance, class, "") %in% 
              c("numeric", "integer"))))
    {
        message("Some columns in data$abundance were not numeric or integer.")
        return(FALSE)
    }

    # check if top-level has covariates
    if ("covariates" %in% names(data))
    {
        # check that data$covariates is a data.frame
        if (!is.data.frame(data$covariates))
        {
            message("data$covariates is not a data.frame.")
            return(FALSE)
        }
        
        # check that data$covariates has the correct number of rows
        if (NROW(data$covariates) != NROW(data$abundance))
        {
                message("data$covariates has a different number of rows than data$abundance.")
                return(FALSE)
        }
    }
    
    return(TRUE)
}