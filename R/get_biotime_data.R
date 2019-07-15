#' @title Get a cleaned BioTime dataset
#' @description Gets a prepped BioTime dataset (as a list of abundance, 
#'   covariates, and metadata) for a specified dataset_id. First run 
#'   \code{\link{prepare_biotime_data}} to create these files from the raw 
#'   BioTime database. If the files are not found, then `NULL` is returned.
#'   Original data found here http://biotime.st-andrews.ac.uk/home.php
#' @param dataset_id the dataset index
#' @inheritParams get_mtquad_data
#' @return list of abundance, covariates, and metadata
#' @examples 
#' \dontrun{
#'   get_biotime_data(dataset_id = 321)
#' }
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

#' @title Return BioTime dataset ids for individual loading
#'
#' @description Retrieve the dataset ids from processed BioTime files. If the 
#'   processed files do not exist, and `do_processing == TRUE`, then we also 
#'   load the raw BioTime database and process the necessary datasets, too.
#' @inheritParams build_biotime_datasets_plan
#' @return vector of dataset ids in the processed set of files
#'
#' @examples
#' \dontrun{
#'   get_biotime_dataset_ids()
#' }
#' @export
get_biotime_dataset_ids <- function(path = get_default_data_path(), data_subset = NULL, 
                                    do_processing = FALSE)
{
    # load in dataset_ids
    storage_path <- file.path(path, "biotime-prepped")
    biotime_citations_file <- file.path(storage_path, "datasets.csv")
    biotime_is_processed <- file.exists(biotime_citations_file)
    if (biotime_is_processed)
    {
        dataset_list <- utils::read.csv(biotime_citations_file, colClasses = "character")
    } else {
        dataset_file <- file.path(path, "biotimesql", "biotimesql_citation1.csv")
        dataset_list <- utils::read.csv(dataset_file, colClasses = "character")
    }
    
    # filter selected datasets
    dataset_ids <- unique(dataset_list$study_id)
    if (!is.null(data_subset)) {
        dataset_ids <- dataset_ids[data_subset]
    }
    
    # process selected datasets if requested
    if (!biotime_is_processed && do_processing)
    {
        message("preprocessing biotime timeseries data")
        if (!dir.exists(storage_path)) {dir.create(storage_path)}
        
        utils::write.csv(dataset_list, 
                         file.path(storage_path, "datasets.csv"), 
                         row.names = F)
        biotime_data_tables <- import_retriever_data("biotimesql", path = path)
        
        purrr::walk(dataset_ids, function(dataset_id) {
            biotime_data_tables %>% 
                process_biotime_data(dataset_id = dataset_id) %>%
                saveRDS(file = file.path(storage_path, paste0("dataset", dataset_id, ".Rds")))
        })
    }
    
    return(dataset_ids)
}

#' @rdname get_biotime_dataset_ids
#' @description `prepare_biotime_data` is a thin wrapper around 
#'   `get_biotime_dataset_ids()` for processing BioTime dataset
#' @inheritParams get_biotime_dataset_ids
#' @return vector of dataset ids in the processed set of files
#'
#' @examples
#' \dontrun{
#'   prepare_biotime_data()
#' }
#' @export
prepare_biotime_data <- function(path = get_default_data_path(), data_subset = NULL)
{
    get_biotime_dataset_ids(do_processing = TRUE)
}

#' @title Process an individual BioTime dataset
#' @description Filter and modify the BioTime data. Generate the abundance, 
#'   covariate, and metadata tables and return the combined object.
#' @param biotime_data_tables full BioTime data tables
#' @param dataset_id the study_id
#' @return the processed BioTime dataset
#' @export
process_biotime_data <- function(biotime_data_tables, dataset_id = 10)
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
