#' @title Invoke an analysis on a dataset
#' 
#' @description This function is a helper that, at its core, simply applies the 
#'   function to the dataset. The key added functionality is to preserve the 
#'   names of the function and the dataset, as well as metadata and any 
#'   additional arguments; returning the result in a tibble that is consistent 
#'   in format, regardless of what function is actually being invoked.
#'   
#' @param fun the analysis function
#' @param data the dataset
#' @param ... additional arguments to pass to `fun`
#' 
#' @return a tibble with these columns:
#'   \tabular{ll}{
#'     \code{results} \tab the output of `fun(data)`\cr
#'     \code{metadata} \tab the metadata component of the original dataset\cr
#'     \code{dataset} \tab the name of the dataset\cr
#'     \code{method} \tab the name of the analysis function\cr
#'     \code{args} \tab a list of optional args to `method`\cr
#'   }
#' 
#' @examples
#' \dontrun{
#'   sgs_data <- MATSS::get_sgs_data()
#'   invoke(ts_summary, sgs_data)
#' }
#' 
#' @export
#' 
invoke <- function(fun, data, ...)
{
    # Get the name of the dataset and method
    dataset_name <- tail(all.vars(match.call()$data), 1)
    method_name <- tail(all.vars(match.call()$fun), 1)
    
    # Extract the metadata from the original dataset
    metadata <- data$metadata
    
    results <- fun(data, ...)
    
    # Return the combined results and metadata
    tibble::tibble(results = list(results), 
                   metadata = list(metadata), 
                   dataset = dataset_name, 
                   method = method_name, 
                   args = list(list(...)))
}

#' @title Create a function that replicates an analysis for all time series in a
#'   dataset
#' 
#' @description This wrapper takes as input, some analysis function, `fun`, and 
#'   returns a function that will run that analysis function on all the time 
#'   series in a dataset. Some post-processing attempts to handle merging of 
#'   the output in a sensible way.
#'   
#' @param fun the analysis function
#' 
#' @return a function that takes in a `dataset` and optional arguments, and 
#'  returns a data.frame or tibble with the combined results, and an "id" 
#'  column with the name of the species
#' 
#' @examples
#' \dontrun{
#'   sgs_data <- MATSS::get_sgs_data()
#'   summarize_dataset <- analysis_wrapper(ts_summary)
#'   summarize_dataset(sgs_data)
#' }
#' 
#' @export
#' 
analysis_wrapper <- function(fun)
{
    # Make sure `fun` is availabe in the returned function
    force(fun)
    
    function(dataset, ...)
    {
        # apply the analysis to each abundance time series
        raw_results <- lapply(dataset$abundance, fun, ...)
        
        # check output types to see if we need conversion
        #   - if all data.frames, then bind_rows
        if (all(purrr::map_lgl(raw_results, ~ "data.frame" %in% class(.))))
        {
            results <- dplyr::bind_rows(raw_results, .id = "id")
            
            #   - if all vectors, then convert to data.frames and bind_rows
        } else if (all(purrr::map_lgl(raw_results, is.vector))) {
            results <- purrr::map(raw_results, ~ tibble::as_tibble(as.list(.), 
                                                                   .name_repair = "unique")) %>%
                dplyr::bind_rows(.id = "id")
            
            #   - otherwise, store as a tibble, with output in a list-column
        } else {
            results <- tibble::tibble("id" = names(raw_results), 
                                      "value" = unname(raw_results))
        }
        
        return(results)
    }
}

