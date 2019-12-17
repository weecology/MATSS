#' @title Create a function that replicates an analysis for all time series in a
#'   dataset
#' 
#' @description This wrapper takes as input, some analysis function, `fun`, 
#'   along with additional optional arguments, and returns a function that will 
#'   run that analysis function on all the time series in a dataset. The return 
#'   object from that function combined all the relevant output, and also 
#'   information on the call, for post-processing that is suitable by itself, or 
#'   as part of a Drake plan.
#'   
#' @param fun the analysis function
#' @param ... extra params to be passed to the analysis function
#' 
#' @return a function that takes in a single argument, `dataset`, and returns a 
#'   tibble with these columns:
#'   \tabular{ll}{
#'     \code{results} \tab a combined results data.frame\cr
#'     \code{metadata} \tab the metadata component of the original dataset\cr
#'     \code{dataset} \tab the name of the dataset\cr
#'     \code{method} \tab the name of the analysis function\cr
#'     \code{args} \tab a list of optional args to `method`\cr
#'   }
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
analysis_wrapper <- function(fun, ...)
{
    # Get the analysis method
    method_name <- all.vars(match.call()$fun)
    
    function(dataset)
    {
        # Get the name of the dataset 
        dataset_name <- all.vars(match.call()$dataset)

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
        
        # Extract the metadata from the original dataset
        metadata <- dataset$metadata
        
        # Return the combined results and metadata
        tibble::tibble(results = list(results), 
                       metadata = list(metadata), 
                       dataset = dataset_name, 
                       method = method_name, 
                       args = list(list(...)))
    }
}
