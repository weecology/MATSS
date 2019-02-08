#' @title Fetch a dataset from data retriever
#'
#' @description Load data, and optionally save to data directory
#'
#' @param name Name of dataset to load
#' @param save Logical If TRUE, save to data directory, in addition to loading
#' @return A list, all data tables associated with named dataset
#' @examples
#' \dontrun{
#'   get_retriever_data("portal", save = FALSE)
#' }
#' @export
#'

get_retriever_data <- function(name, save = FALSE)
{
    
    if(save) { 
        folder_path <- here::here("data", name)
        dir.create(folder_path)
        rdataretriever::install(name, "csv", 
                                data_dir = folder_path)
    }
    
    rdataretriever::fetch(name)
    
}

#' @title Load all retriever data
#'
#' @description Load all useable data from retriever
#'
#' @param names List of dataset names (from retriever) to load
#' @return A list of lists, all datasets compiled and retaining their retriever names
#' @examples
#' \dontrun{
#'   retriever_data(c("portal","veg-plots-sdl"))
#' }
#' 
#' @export
#'

retriever_data <- function(names = c('breed-bird-survey',
                                     'mapped-plant-quads-mt',
                                     'fray-jorge-ecology',
                                     'veg-plots-sdl'))
{
    
    retriever <- sapply(names,rdataretriever::fetch, USE.NAMES = TRUE)
    
    return(retriever)
    
}
