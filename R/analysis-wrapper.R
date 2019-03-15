#' @title Create a function that replicates an analysis for all time series in a
#'   dataset
#' 
#' @description This wrapper takes as input, an analysis function intended to be
#'   applied to a single time series, and creates a function that will apply 
#'   that analysis to all the time series as part of a dataset, returning a 
#'   combined object with all of the results.
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
#'   summarize_dataset <- forecast_wrapper(ts_summary)
#'   summarize_dataset(sgs_data)
#' }
#' 
#' @export
#' 
analysis_wrapper <- function(fun, ...)
{
    # Get the forecast method
    method_name <- all.vars(match.call()$fun)
    
    function(dataset)
    {
        # Get the name of the dataset 
        dataset_name <- all.vars(match.call()$dataset)

        # Make the forecasts
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
