#' @title get Montana plant quad time-series data
#'
#' @param path where to load the raw data files from
#' @return list of abundance, covariates, and metadata
#' 
#' @export

get_mtquad_data <- function(path = file.path(get_default_data_path(), 
                                             "mapped-plant-quads-mt"))
{
    mtquad_data_tables <- import_retriever_data(path = path)
    
    mtquad_data <- mtquad_data_tables$mapped_plant_quads_mt_allrecords_density %>%
        dplyr::select_at(dplyr::vars(-c("objectid", "seedling", "x", "y"))) %>%
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
