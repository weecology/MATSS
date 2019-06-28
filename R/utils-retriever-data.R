#' @title Check if a default data path is set
#' 
#' @description See \code{portalr::\link[portalr]{check_default_data_path}} for details.
#' 
#' @export
check_default_data_path <- function()
{
    portalr::check_default_data_path(ENV_VAR = "MATSS_DATA_PATH")
}

#' @title What is the default data path?
#'
#' @description See \code{portalr::\link[portalr]{get_default_data_path}} for details.
#' @inheritParams portalr::get_default_data_path
#'
#' @export
get_default_data_path <- function(fallback = "~")
{
    portalr::get_default_data_path(fallback, ENV_VAR = "MATSS_DATA_PATH")
}

#' @title Manage the default path for downloading MATSS Data into
#'
#' @description See \code{portalr::\link[portalr]{use_default_data_path}} for details.
#' @inheritParams portalr::use_default_data_path
#'
#' @export
use_default_data_path <- function(path = NULL)
{
    portalr::use_default_data_path(path, ENV_VAR = "MATSS_DATA_PATH")
}

#' @title Download a dataset from data retriever
#' @aliases import_retriever_data
#'  
#' @description \code{install_retriever_data} downloads retriever datasets and 
#'   is a wrapper around rdataretriever::install 
#'   
#' @param path the overarching folder in which to download datasets
#' @param force_install whether to install the dataset if the correctly named 
#'   folder already exists
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
install_retriever_data <- function(dataset, path = get_default_data_path(), 
                                   force_install = FALSE)
{
    # check for existence of data_path
    path <- normalizePath(path, mustWork = TRUE)
    
    # where to put the retriever data
    folder_path <- file.path(path, dataset)
    if (dir.exists(folder_path) && !force_install)
    {
        message("A folder already exists for \"", dataset, "\".\n", 
                "Use `force_install = TRUE` to overwrite it with a fresh install.")
    } else {
        # make the folder
        dir.create(folder_path)
        
        # install the retriever data
        tryCatch(rdataretriever::install_csv(dataset, data_dir = folder_path), 
                 error = function(e) {
                     unlink(folder_path, recursive = TRUE)
                     e
                 }
        )
    }
}

#' @title Import a dataset downloaded from data retriever
#' @rdname install_retriever_data
#'
#' @description \code{import_retriever_data} loads a previously downloaded 
#'   retriever dataset
#'   
#' @inheritParams install_retriever_data
#' 
#' @return NULL
#' 
#' @examples
#' \dontrun{
#'   import_retriever_data("veg-plots-sdl")
#' }
#' @export
#'
import_retriever_data <- function(dataset, path = get_default_data_path())
{
    folder_path <- file.path(path, dataset)
    
    # check for existence of data_path
    files <- dir(folder_path)
    if (length(files) == 0) # check for presence of downloaded files
    {
        warning("Didn't find any downloaded data in ", folder_path, ".\n", 
                "Did you run get_retriever_data() first?")
        return(NULL)
    }
    
    # load each csv and return a list
    tempdata <- vector('list', length(files))
    names(tempdata) <- sub('.csv', '', files)
    for (j in seq_along(files))
    {
        tempdata[[j]] <- utils::read.csv(file.path(folder_path, files[j]), 
                                         stringsAsFactors = FALSE)
    }
    return(tempdata)
}