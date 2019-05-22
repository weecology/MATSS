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
        results <- purrr::map_dfr(dataset$abundance, fun, .id = "id", ...)
        
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
