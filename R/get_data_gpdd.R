#' @title Get a cleaned GPDD dataset
#' @description Gets a prepped GPDD dataset (as a list of abundance, 
#'   covariates, and metadata) for a specified `location_id` and `timeperiod_id`
#'   First run [`prepare_gpdd_data`] to create these files from the raw 
#'   GPDD database. If the files are not found, then `NULL` is returned.
#'   
#' @param location_id Location code of data to return
#' @param timeperiod_id Sampling timescale code of data to return 
#'   (some datasets provide at more than one scale)
#' @inheritParams get_mtquad_data
#' @return list of abundance, covariates, and metadata
#' 
#' @export
get_gpdd_data <- function(path = file.path(get_default_data_path(), "gpdd-prepped", 
                                           paste0("location", location_id, ".RDS")), 
                          location_id = 83)
{
    if (!is.numeric(location_id))
    {
        location_id <- as.numeric(location_id)
    }
    
    if (file.exists(path)) {
        return(readRDS(path)) 
    } else {
        return(NULL)
    }
}

#' @title Prepare GPDD population time-series data
#' @description Selects sites containing at least `min_num_yrs` of data 
#'   samples during that period. Cleans data tables and stores each individual 
#'   community dataset as a .RDS file. Saves a data table of the `location_id`s.
#'
#' @param data_subset optional, a subset of the GPDD communities to use 
#'   (to speed up development). As c(1:X)
#' @param min_num_yrs minimum number of years of data
#' @inheritParams get_mtquad_data
#' @return NULL
#' 
#' @export
prepare_gpdd_data <- function(path = get_default_data_path(), data_subset = NULL, 
                              min_num_yrs = 10)
{
    # get locations
    locations_file <- system.file("extdata", "gpdd_locations.csv", 
                                  package = "MATSS", mustWork = TRUE)
    locations <- utils::read.csv(locations_file, colClasses = "character") %>%
        dplyr::rename(location_id = .data$LocationID, timeperiod_id = .data$TimePeriodID)
    if (!is.null(data_subset)) {
        locations <- locations[data_subset, ]
    }
    stopifnot(length(unique(locations$location_id)) == NROW(locations))
    
    # write locations to storage_path
    storage_path <- file.path(path, "gpdd-prepped")
    if (!dir.exists(storage_path)) {
        dir.create(storage_path)
    }
    utils::write.csv(locations, 
                     file.path(storage_path, "locations.csv"), 
                     row.names = F)
    
    # initial pre-processing
    gpdd_data_tables <- import_retriever_data("global-population-dynamics", path = path)
    citation_file <- file.path(path, "global-population-dynamics", "CITATION")
    citation_text <- readLines(citation_file, warn = FALSE)
    
    global_species_table <- gpdd_data_tables$global_population_dynamics_taxon %>% 
        dplyr::rename(id = .data$taxonid) %>%
        dplyr::mutate(id = paste0("sp", .data$id))
    
    datasource_table <- gpdd_data_tables$global_population_dynamics_datasource
    
    gpdd_data_raw <- gpdd_data_tables$global_population_dynamics_data %>%
        dplyr::left_join(gpdd_data_tables$global_population_dynamics_main, by = "mainid") %>%
        dplyr::left_join(gpdd_data_tables$global_population_dynamics_location, by = "locationid") %>%
        dplyr::mutate(taxonid = paste0("sp", .data$taxonid), 
                      date = format(lubridate::date_decimal(.data$decimalyearbegin), "%Y-%m-%d")) %>%
        dplyr::select(-tidyselect::all_of(c("siblyfittedtheta", "siblythetacilower", "siblythetaciupper", "siblyextremeneffect",    
                                            "siblyreturnrate", "siblycarryingcapacity", "population", "generation", 
                                            "spatialdensity", "spatialaccuracy")))
    
    # process individual lcoations
    locations %>%
        purrr::pwalk(function(location_id, timeperiod_id) {
            if (timeperiod_id == "1:12")
            {
                timeperiod_id <- 1:12
            }
            gpdd_data <- gpdd_data_raw %>%
                dplyr::filter(.data$locationid == location_id, 
                              .data$timeperiodid %in% timeperiod_id,
                              .data$datasetlength >= min_num_yrs) %>%
                dplyr::arrange(.data$date)
            
            if (NROW(gpdd_data) == 0) # check for empty data
                return();
            
            process_gpdd_data(gpdd_data,
                              global_species_table, 
                              datasource_table, 
                              save_to_file = TRUE, 
                              storage_path = storage_path, 
                              citation_text = citation_text)
        })

    invisible()
}

#' @title Process the GPDD data for an individual location and timeperio
#' @description Generate the abundance, covariate, and metadata tables and 
#'   return the combined object.
#'   
#' @param gpdd main GPDD data table
#' @param global_species_table table of species for GPDD
#' @param datasource_table table of data sources for GPDD
#' @inheritParams process_biotime_dataset
#' @return the processed GPDD dataset
#' 
#' @export
process_gpdd_data <- function(gpdd_data,
                              global_species_table, 
                              datasource_table, 
                              save_to_file = FALSE, 
                              storage_path = file.path(get_default_data_path(), 
                                                       "gpdd-prepped"), 
                              citation_text = NULL)
{
    # check that exactly one location is represented in the data
    locationid <- unique(gpdd_data$locationid)
    stopifnot(length(locationid) == 1)
    
    summary_by_date_and_taxonid <- gpdd_data %>% 
        dplyr::select(.data$date, .data$sampleyear, .data$decimalyearbegin,
                      .data$decimalyearend, .data$taxonid, .data$populationuntransformed) %>%
        dplyr::group_by(.data$date, .data$taxonid) %>%
        dplyr::summarize(total = sum(.data$populationuntransformed), 
                         sampleyear = mean(.data$sampleyear), 
                         decimalyearbegin = min(.data$decimalyearbegin), 
                         decimalyearend = max(.data$decimalyearend)) %>%
        tidyr::spread(key = .data$taxonid, value = .data$total, fill = 0) %>%
        dplyr::ungroup()
    
    abundance <- summary_by_date_and_taxonid %>%
        dplyr::select(dplyr::starts_with("sp"))
    
    covariates <- summary_by_date_and_taxonid %>%
        dplyr::mutate_at("date", as.Date) %>% 
        dplyr::select(-dplyr::starts_with("sp"))
    
    location <- gpdd_data %>% 
        dplyr::select_at(c("biotopeid", "locationid", "exactname", "townname", "countystateprovince", 
                           "country", "continent", "ocean", "longitudedegrees", "longitudeminutes", "eorw", 
                           "latitudedegrees", "latitudeminutes", "nors", "longdd", "latdd", 
                           "north", "east", "south", "area", "notes.y", "locationextent")) %>% 
        dplyr::rename(latitude = .data$latdd, 
                      longitude = .data$longdd) %>%
        dplyr::distinct()
    
    samples <- gpdd_data %>% 
        dplyr::select_at(c("samplingfrequency", "startyear", "endyear", "samplingunits", 
                           "samplingprotocol", "reliability", "datasetlength", "notes.x", "notes.y")) %>% 
        dplyr::distinct()
    if (samples$samplingfrequency[1] == 1)
    {
        timename <- "sampleyear"
    } else if (samples$samplingfrequency[1] == 12) {
        timename <- "decimalyearbegin"
    } else {
        stop("help ", locationid)
    }
    
    source <- gpdd_data %>% 
        dplyr::select(.data$datasourceid) %>% 
        dplyr::distinct() %>%
        dplyr::left_join(datasource_table, by = "datasourceid")
    
    species_table <- dplyr::filter(global_species_table, .data$id %in% colnames(abundance))
    
    citation_line <- paste(source$author, source$year, source$title, source$reference, sep = ". ")
    
    metadata <- list(timename = timename, 
                     effort = NULL, 
                     period = 1/as.numeric(samples$samplingfrequency[1]), 
                     species_table = species_table, 
                     location = location, 
                     samples = samples, 
                     source = source, 
                     is_community = FALSE, 
                     citation = c(citation_line, citation_text))
    
    out <- list("abundance" = abundance, 
                "covariates" = covariates, 
                "metadata" = metadata)
    attr(out, "class") <- "matssdata"
    
    if (!save_to_file)
    {
        return(out)
    }
    saveRDS(out, file = file.path(storage_path, paste0("location", locationid, ".RDS")))
    invisible()
}
