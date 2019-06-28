#' @title Get Biotime time-series data
#'
#' @description Original data found here http://biotime.st-andrews.ac.uk/home.php
#' @param dataset Dataset number to retrieve
#' @param path Data storage path
#'
#' @return list of two dataframes (one with abundance data, the other with covariate data) 
#'   and one list of metadata.
#'
#' @examples
#' \dontrun{
#'   get_biotime_data(dataset = 321)
#' }
#' @export

get_biotime_data <- function(dataset, path = get_default_data_path())
{
    biotime_data_tables <- import_retriever_data('biotimesql', path = path)
    
    biotime_citations <- biotime_data_tables$biotimesql_citation1 %>%
        dplyr::filter(study_id == dataset)
    
    biotime_data <- biotime_data_tables$biotimesql_allrawdata %>%
        dplyr::filter(study_id == dataset) %>%
        dplyr::select(-dplyr::one_of(c("day", "sample_desc", "biomass", 
                                       "id_all_raw_data", "depth", "study_id"))) %>%
        dplyr::arrange(.data$year, .data$month)
    
    abundance <- biotime_data %>%
        dplyr::group_by(.data$year, .data$month, .data$id_species) %>%
        dplyr::summarise(abundance = sum(.data$abundance)) %>%
        tidyr::spread(key = .data$id_species, value = .data$abundance, fill = 0) %>%
        dplyr::ungroup() %>%
        dplyr::select(-c(.data$month, .data$year))
    
    covariates <- biotime_data %>%
        dplyr::group_by(.data$year, .data$month) %>%
        dplyr::summarise(effort = length(unique(plot)),
                         latitude = mean(latitude, na.rm = TRUE),
                         longitude = mean(longitude, na.rm = TRUE)) %>%
        dplyr::ungroup() %>%
        dplyr::mutate(month=tidyr::replace_na(month, 1),
                      date=lubridate::as_date(paste(year, month, 1)))
    
    metadata <- list(timename = "date", effort = "effort", 
                     source = biotime_citations$citation_line)
    return(list('abundance' = abundance, 'covariates' = covariates, 
                'metadata' = metadata))
}
