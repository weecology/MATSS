#' @title Download a dataset from data retriever
#'
#' @description a wrapper around rdataretriever::install to facilitate 
#'   downloading multiple datasets into a common data folder
#'   
#' @param data_path the overarching folder in which to download datasets
#' @inheritParams rdataretriever::install
#' 
#' @return NULL
#' 
#' @examples
#' \dontrun{
#'   install_retriever_data("veg-plots-sdl")
#' }
#' @export
#'

install_retriever_data <- function(dataset, data_path = "data")
{
    # check for existence of data_path
    if(!dir.exists(here::here('data'))) {
        dir.create(here::here('data'))
    }
    folder_path <- file.path(here::here(data_path, dataset))
    dir.create(folder_path)
    rdataretriever::install(dataset, "csv", 
                            data_dir = folder_path)
}

#' @title Import a dataset downloaded from data retriever
#'
#' @description a function to import a previously downloaded dataset from 
#'   rdataretriever
#'   
#' @param data_path the overarching folder in which to import datasets from
#' @inheritParams rdataretriever::install
#' 
#' @return NULL
#' 
#' @examples
#' \dontrun{
#'   import_retriever_data("veg-plots-sdl")
#' }
#' @export
#'
import_retriever_data <- function(dataset, data_path = "data")
{
    # check for existence of data_path

    folder_path <- file.path(data_path, dataset)
    
    files <- dir(folder_path)
    if (length(files) == 0) # check for presence of downloaded files
    {
        warning("Didn't find any downloaded data in ", folder_path, ".\n", 
                "Did you run get_retriever_data() first?")
        return(NULL)
    }
    
    tempdata <- vector('list', length(files))
    names(tempdata) <- sub('.csv', '', files)
    for (j in seq_along(files))
    {
        tempdata[[j]] <- utils::read.csv(file.path(folder_path, files[j]))
    }
    return(tempdata)
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
