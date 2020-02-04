#' @title Create GPDD population time-series data
#'
#' @description Selects sites containing at least `min_num_yrs`` of data 
#'   samples during that period.
#'
#' @param min_num_yrs minimum number of years of data
#' @param location_id Location code of data to return
#' @param timeperiod_id Sampling timescale code of data to return 
#' (some datasets provide at more than one scale)
#' @inheritParams get_mtquad_data
#' 
#' @return list of abundance, covariates, and metadata
#' 
#' @examples
#' \dontrun{
#'   get_gpdd_data(location_id=83, timeperiod_id=408)
#' }
#' @export

get_gpdd_data <- function(path = get_default_data_path(), 
                          location_id = 83, timeperiod_id = 408, min_num_yrs = 10)
{
    
    gpdd_data_tables <- import_retriever_data("global-population-dynamics", path = path)
    citation_file <- file.path(path, "global-population-dynamics", "CITATION")
    citation_text <- readLines(citation_file, warn = FALSE)
    
    gpdd_data <- gpdd_data_tables$global_population_dynamics_data %>%
        dplyr::left_join(gpdd_data_tables$global_population_dynamics_main, by = "mainid") %>%
        dplyr::left_join(gpdd_data_tables$global_population_dynamics_location, by = "locationid") %>%
        dplyr::filter(.data$locationid == location_id, .data$timeperiodid %in% timeperiod_id,
                      .data$datasetlength >= min_num_yrs) %>%
        dplyr::mutate(taxonid = paste0("sp", .data$taxonid), 
                      date = format(lubridate::date_decimal(.data$decimalyearbegin), "%Y-%m-%d")) %>%
        dplyr::arrange(.data$date) %>%
        dplyr::select(-dplyr::one_of(c("siblyfittedtheta", "siblythetacilower", "siblythetaciupper", "siblyextremeneffect",    
                                       "siblyreturnrate", "siblycarryingcapacity", "population", "generation", 
                                       "spatialdensity", "spatialaccuracy")))
    
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
    
    location = gpdd_data %>% 
        dplyr::select_at(c("biotopeid", "locationid", "exactname", "townname", "countystateprovince", 
                           "country", "continent", "ocean", "longitudeminutes", "eorw", 
                           "latitudedegrees", "latitudeminutes", "nors", "longdd", "latdd", 
                           "north", "east", "south", "area", "notes.y", "locationextent")) %>% 
        dplyr::distinct()
    
    samples = gpdd_data %>% 
        dplyr::select_at(c("samplingfrequency", "startyear", "endyear", "samplingunits", 
                           "samplingprotocol", "reliability", "datasetlength", "notes.x", "notes.y")) %>% 
        dplyr::distinct()
    
    source = gpdd_data %>% 
        dplyr::select(.data$datasourceid) %>% 
        dplyr::distinct() %>%
        dplyr::left_join(gpdd_data_tables$global_population_dynamics_datasource, by = "datasourceid")
    
    species_table <- gpdd_data_tables$global_population_dynamics_taxon %>% 
        dplyr::rename(id = .data$taxonid) %>%
        dplyr::mutate(id = paste0("sp", .data$id)) %>%
        dplyr::filter(.data$id %in% colnames(abundance))
    
    citation_line <- paste(source$author, source$year, source$title, source$reference, sep = ". ")
    
    metadata <- list(timename = "date", 
                     species_table = species_table, 
                     effort = NULL, 
                     location = location, 
                     samples = samples, 
                     source = source, 
                     citation = c(citation_line, citation_text))
    
    out <- list("abundance" = abundance, 
                "covariates" = covariates, 
                "metadata" = metadata)
    
    return(out)
}