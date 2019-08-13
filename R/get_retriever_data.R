#' @title Create Sonoran desert lab time-series data
#'
#' @description Original data found here http://www.eebweb.arizona.edu/faculty/venable/LTREB/LTREB%20data.htm
#'
#' @param plots vector of plots to keep
#' @inheritParams get_mtquad_data
#'
#' @return list of two dataframes (one with abundance data, the other with covariate data) 
#'   and one list of metadata.
#'
#' @examples
#' \dontrun{
#'   get_sdl_data(sdl_data_tables=retriever_data()$'veg-plots-sdl')
#' }
#' @export

get_sdl_data <- function(plots = c(4, 7, 8, 9, 10, 11, 12, 14, 15, 16, 17), 
                         path = get_default_data_path())
{
    sdl_data_tables <- import_retriever_data("veg-plots-sdl", path = path)
    
    sdl_data <- sdl_data_tables$veg_plots_sdl_SMDensity %>%
        dplyr::select(-.data$countns) %>%
        dplyr::filter(.data$plot %in% plots) %>%
        dplyr::group_by(.data$year, .data$code) %>%
        dplyr::summarize(count = sum(.data$count)) %>%
        tidyr::spread(key = .data$code, value = .data$count, fill = 0) %>%
        dplyr::rename(UNKN = .data$V1) %>%
        dplyr::ungroup()
    
    abundance <- dplyr::select(sdl_data, -.data$year)
    covariates <- dplyr::select(sdl_data, .data$year)
    metadata <- list(timename = "year", effort = NULL)
    
    return(list('abundance' = abundance, 'covariates' = covariates, 
                "metadata" = metadata))
}

#' @title Create Montana plant quad time-series data
#'
#' @description Original data found here 
#'
#' @param path where to load the raw data files from
#'
#' @return list of two dataframes (one with abundance data, the other with covariate data) 
#'   and one list of metadata.
#' 
#' @examples
#' \dontrun{
#'   get_mtquad_data(mtquad_data_tables=retriever_data()$'mapped-plant-quads-mt')
#' }
#' @export

get_mtquad_data <- function(path = file.path(get_default_data_path(), "mapped-plant-quads-mt"))
{
    mtquad_data_tables <- import_retriever_data(path = path)
    
    mtquad_data <- mtquad_data_tables$mapped_plant_quads_mt_allrecords_density %>%
        dplyr::select(-.data$objectid, -.data$seedling, -.data$x, -.data$y) %>%
        dplyr::group_by(.data$year, .data$species, .data$quad) %>%
        dplyr::summarize(abundance = sum(.data$stems)) %>%
        dplyr::group_by(.data$year, .data$species) %>%
        dplyr::summarize(abundance = sum(.data$abundance)) %>%
        tidyr::spread(key = .data$species, value = .data$abundance, fill = 0) %>%
        dplyr::ungroup()
    
    abundance <- dplyr::select(mtquad_data, -.data$year)
    
    covariates <- dplyr::select(mtquad_data, .data$year)
    
    metadata <- list(timename = "year", effort = NULL)
    return(list('abundance' = abundance, 'covariates' = covariates, 
                "metadata" = metadata))
}
