#' @title Get a cleaned BioTime dataset
#' @description Gets a prepped BioTime dataset (as a list of abundance, 
#'   covariates, and metadata) for a specified dataset_id. First run 
#'   \code{\link{prepare_biotime_data}} to create these files from the raw 
#'   BioTime database. If the files are not found, then `NULL` is returned.
#' @param dataset_id the dataset index
#' @inheritParams get_mtquad_data
#' @return list of abundance, covariates, and metadata
#' @export
get_biotime_data <- function(dataset_id, path = get_default_data_path())
{
    this_path <- file.path(path, "biotime-prepped", 
                           paste0("dataset", dataset_id, ".Rds"))
    if (file.exists(this_path)) {
        return(readRDS(this_path)) 
    } else {
        return(NULL)
    }
}

#' @title Prepare Biotime time-series datases for individual loading
#'
#' @description Original data found here http://biotime.st-andrews.ac.uk/home.php
#' @param data_subset optional, a subset of the Biotime study_ids to use 
#'   (to speed up development). As c(1:X)
#' @inheritParams get_mtquad_data
#' @return NULL
#'
#' @examples
#' \dontrun{
#'   get_biotime_data(dataset = 321)
#' }
#' @export
prepare_biotime_data <- function(path = get_default_data_path(), data_subset = NULL)
{
    # get dataset ids
    dataset_file <- file.path(path, "biotimesql", "biotimesql_citation1.csv")
    dataset_list <- utils::read.csv(dataset_file, colClasses = "character")
    dataset_ids <- unique(dataset_list$study_id)
    
    # prepare and write out dataset id metadata
    storage_path <- file.path(path, "biotime-prepped")
    if (!dir.exists(storage_path)) {
        dir.create(storage_path)
    }
    
    utils::write.csv(dataset_list, 
                     file.path(storage_path, "dataset_ids.csv"), 
                     row.names = F)
    
    # filter and process selected datasets
    if (!is.null(data_subset)) {
        dataset_ids <- dataset_ids[data_subset]
    }
    
    biotime_data_tables <- import_retriever_data("biotimesql", path = path)
    
    purrr::walk(dataset_ids, function(dataset_id) {
        biotime_data_tables %>% 
            process_biotime_data(dataset_id = dataset_id) %>%
            saveRDS(file = file.path(storage_path, paste0("dataset", dataset_id, ".Rds")))
    })
}

#' @title Process an individual BioTime dataset
#' @description Filter and modify the BioTime data. Generate the abundance, 
#'   covariate, and metadata tables and return the combined object.
#' @param biotime_data_tables full BioTime data tables
#' @param dataset_id the study_id
#' @return the processed BioTime dataset
#' @export
process_biotime_data <-  function(biotime_data_tables, dataset_id = 10)
{
    biotime_citations <- biotime_data_tables$biotimesql_citation1 %>%
        dplyr::filter(.data$study_id == dataset_id)
    
    biotime_data <- biotime_data_tables$biotimesql_allrawdata %>%
        dplyr::filter(.data$study_id == dataset_id) %>%
        dplyr::select(-dplyr::one_of(c("day", "sample_desc", "biomass", 
                                       "id_all_raw_data", "depth", "study_id"))) %>%
        dplyr::arrange(.data$year, .data$month)
    
    abundance <- biotime_data %>%
        dplyr::group_by(.data$year, .data$month, .data$id_species) %>%
        dplyr::summarize(abundance = sum(.data$abundance)) %>%
        tidyr::spread(key = .data$id_species, value = .data$abundance, fill = 0) %>%
        dplyr::ungroup() %>%
        dplyr::select(-c(.data$month, .data$year))
    
    covariates <- biotime_data %>%
        dplyr::group_by(.data$year, .data$month) %>%
        dplyr::summarize(effort = length(unique(.data$plot)),
                         latitude = mean(.data$latitude, na.rm = TRUE),
                         longitude = mean(.data$longitude, na.rm = TRUE)) %>%
        dplyr::ungroup() %>%
        dplyr::mutate(month = tidyr::replace_na(.data$month, 1),
                      date = lubridate::as_date(paste(.data$year, .data$month, 1)))
    
    metadata <- list(timename = "date", effort = "effort", 
                     source = biotime_citations$citation_line)
    return(list("abundance" = abundance, "covariates" = covariates, "metadata" = metadata))
}
