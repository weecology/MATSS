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
    
    out <- list("abundance" = abundance, 
                "covariates" = covariates, 
                "metadata" = metadata) %>%
        append_retriever_citation(path)
    
    return(out)
}
