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
install_retriever_data <- function(dataset, path = get_default_data_path())
{
    # check for existence of data_path
    path <- normalizePath(path, mustWork = TRUE)

    # where to put the retriever data
    folder_path <- file.path(path, dataset)
    dir.create(folder_path)
    
    # install the retriever data
    rdataretriever::install(dataset, "csv", 
                            data_dir = folder_path)
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
#' @noRd
#'
retriever_data <- function(names = c('breed-bird-survey',
                                     'mapped-plant-quads-mt',
                                     'fray-jorge-ecology',
                                     'veg-plots-sdl'))
{
    
    retriever <- sapply(names,rdataretriever::fetch, USE.NAMES = TRUE)
    
    return(retriever)
    
}
