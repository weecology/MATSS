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
get_biotime_data <- function(path = get_default_data_path(), 
                             dataset_id = 321, 
                             raw_path = file.path(path, 
                                                  "biotime-prepped", 
                                                  paste0("dataset", dataset_id, ".Rds")))
{
    if (!is.numeric(dataset_id))
    {
        dataset_id <- as.numeric(dataset_id)
    }
    
    if (file.exists(raw_path)) {
        return(readRDS(raw_path)) 
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
get_biotime_dataset_ids <- function(path = get_default_data_path(), 
                                    data_subset = NULL, 
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
            message("  -dataset_id = ", dataset_id)
            process_biotime_dataset(biotime_data_tables, 
                                    dataset_id = dataset_id, 
                                    save_to_file = TRUE, 
                                    storage_path = storage_path)
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
    get_biotime_dataset_ids(path = path, 
                            data_subset = data_subset, 
                            do_processing = TRUE)
}

#' @title Correct and clean specific datasets
#' @details For `dataset_id = 54`, it appears that day and month were sometimes 
#'   interchanged. Since there did not seem to be measurements after August in 
#'   any given year otherwise, we use that to filter and swap `day` and `month`.
#'   
#' @param raw_data The raw data for a specific dataset_id
#' @inheritParams process_biotime_dataset
#' @return a corrected version of `raw_data`
#' 
#' @export
correct_biotime_dataset <- function(raw_data, dataset_id = 10)
{
    switch(as.character(dataset_id), 
           "54" = {
               raw_data %>%
                   dplyr::mutate(temp = day, 
                                 day = ifelse(month >= 9, month, day), 
                                 month = ifelse(month >= 9, temp, month), 
                                 temp = NULL)
           },
           "327" = {
               pattern <- "[0-9]+_[0-9]+_[0-9]+_([0-9]+-[0-9]+-[0-9]+)"
               dates_from_desc <- stringr::str_match(raw_data$sample_desc, 
                                                     pattern)[, 2] %>%
                   lubridate::as_date()
               raw_data %>%
                   dplyr::mutate(day = lubridate::day(dates_from_desc), 
                                 month = lubridate::month(dates_from_desc))
           },
           "373" = {
               raw_data %>%
                   dplyr::mutate(day = dplyr::if_else(day == 0, NA_integer_, day), 
                                 month = dplyr::if_else(month == 0, NA_integer_, month))
           }, 
           "511" = {
               raw_data %>%
                   dplyr::mutate(month = dplyr::if_else(month == 0, NA_integer_, month))
               
           }, 
           {
               # 302, 330, 342, 343, 344, 345, 346, 352, 
               # 380, 381, 382, 383, 384, 385, 386, 387, 
               # 388, 389, 390, 391, 392, 393, 394, 395, 
               # 396, 397, 398, 399, 400, 401
               raw_data %>%
                   dplyr::mutate(day = NA, 
                                 month = NA)
               # if (all(!is.finite(raw_data$month)) || 
               #     is.numeric(raw_data$month) && 
               #     all(raw_data$month < 1 | 
               #         raw_data$month > 12))
               #     raw_data$month <- 1
               # 
               # raw_data
           }
    )
}

#' @title Process an individual BioTime dataset
#' @description Filter and modify the BioTime data. Generate the abundance, 
#'   covariate, and metadata tables and return the combined object.
#' @param biotime_data_tables full BioTime data tables
#' @param dataset_id the study_id
#' @param save_to_file whether to save the processed dataset to a file
#' @param storage_path folder in which to put processed dataset
#' @return the processed BioTime dataset
#' @export
process_biotime_dataset <- function(biotime_data_tables, 
                                    dataset_id = 10, 
                                    save_to_file = FALSE, 
                                    storage_path = file.path(get_default_data_path(), 
                                                             "biotime-prepped"))
{
    raw_data <- biotime_data_tables$biotimesql_allrawdata %>%
        dplyr::filter(.data$study_id == dataset_id) %>% 
        correct_biotime_dataset(dataset_id = dataset_id)
    
    biotime_data <- raw_data %>%
        dplyr::select(-dplyr::one_of(c("day", "sample_desc", "biomass", 
                                       "id_all_raw_data", "study_id"))) %>%
        dplyr::arrange(.data$year, .data$month)
    
    # check if samples are only annual
    is_annual_sampling <- all(is.na(biotime_data$month))
    if (is_annual_sampling)
    {
        grouping <- c("year")
    } else {
        grouping <- c("year", "month")
    }
    
    abundance <- biotime_data %>%
        dplyr::group_by_at(c(grouping, "id_species")) %>%
        dplyr::summarize(abundance = sum(.data$abundance)) %>%
        tidyr::spread(key = .data$id_species, value = .data$abundance, fill = 0) %>%
        dplyr::ungroup() %>%
        dplyr::select_at(dplyr::vars(-grouping))
    
    covariates <- biotime_data %>%
        dplyr::group_by_at(grouping) %>%
        dplyr::summarize(effort = length(unique(.data$plot)),
                         latitude = mean(.data$latitude, na.rm = TRUE),
                         longitude = mean(.data$longitude, na.rm = TRUE), 
                         depth = mean(.data$depth, na.rm = TRUE)) %>%
        dplyr::ungroup()
    if (is_annual_sampling)
    {
        covariates <- covariates %>%
            dplyr::mutate(date = lubridate::as_date(paste(.data$year, 1, 1)))
    } else {
        covariates <- covariates %>%
            dplyr::mutate(month = tidyr::replace_na(.data$month, 1),
                          date = lubridate::as_date(paste(.data$year, .data$month, 1)))
    }
    
    site_info <- c(biotime_data_tables$biotimesql_site %>%
                       dplyr::filter(.data$study_id == dataset_id) %>%
                       dplyr::select(-dplyr::one_of(c("study_id", "id_site", "cen_latitude", 
                                                      "cen_longitude", "area"))) %>%
                       as.list(), 
                   biotime_data_tables$biotimesql_datasets %>%
                       dplyr::filter(.data$study_id == dataset_id) %>%
                       dplyr::left_join(biotime_data_tables$biotimesql_ID_ABUNDANCE, 
                                        by = c("ab_type" = "id_abundance")) %>%
                       dplyr::left_join(biotime_data_tables$biotimesql_biomass, 
                                        by = c("bio_type" = "id_biomass")) %>%
                       dplyr::left_join(biotime_data_tables$biotimesql_sample, 
                                        by = c("sample_type" = "id_sample")) %>%
                       dplyr::select(-dplyr::one_of(c("study_id", "ab_type", 
                                                      "bio_type", "sample_type", 
                                                      "id_datasets"))) %>%
                       as.list())
    
    citation_info <- biotime_data_tables$biotimesql_citation1 %>%
        dplyr::filter(.data$study_id == dataset_id) %>%
        dplyr::select(-dplyr::one_of(c("study_id", "id_citation1")))
    contact_info <- biotime_data_tables$biotimesql_contacts %>%
        dplyr::filter(.data$study_id == dataset_id) %>%
        dplyr::select(-dplyr::one_of(c("study_id", "id_contacts")))
    species_table <- biotime_data_tables$biotimesql_species %>%
        dplyr::filter(.data$id_species %in% unique(biotime_data$id_species)) %>%
        dplyr::rename(id = .data$id_species)
    method_info <- biotime_data_tables$biotimesql_methods %>%
        dplyr::filter(.data$study_id == dataset_id)
    
    metadata <- c(list(timename = "date", effort = "effort", 
                       source = citation_info$citation_line, 
                       contact_info = contact_info, 
                       species_table = species_table, 
                       method = method_info$methods, 
                       is_annual_sampling = is_annual_sampling), 
                  site_info)
    
    out <- list("abundance" = abundance, 
                "covariates" = covariates, 
                "metadata" = metadata)
    
    if (save_to_file)
    {
        saveRDS(out, 
                file = file.path(storage_path, 
                                 paste0("dataset", dataset_id, ".Rds")))
    }
    
    return(out)
}
