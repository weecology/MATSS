#' @title Create GPDD population time-series data
#'
#' @description Selects sites containing at 
#' least min_num_yrs of data samples during that period.
#'
#' @param min_num_yrs minimum number of years of data
#' @param location_id Location code of data to return
#' @param timeperiod_id Sampling timescale code of data to return 
#' (some datasets provide at more than one scale)
#'
#' @return list of two dataframes (one with abundance data, the other with covariate data) 
#'   and one list of metadata.
#' 
#' @examples
#' \dontrun{
#'   get_gpdd_data(location_id=83, timeperiod_id=408)
#' }
#' @export

get_gpdd_data <- function(location_id, timeperiod_id, min_num_yrs = 10)
{
    gpdd_data <- rgpdd::gpdd_data %>% 
        dplyr::left_join(rgpdd::gpdd_main, by = "MainID") %>%
        dplyr::left_join(rgpdd::gpdd_location, by = "LocationID") %>%
        dplyr::filter(.data$LocationID == location_id, .data$TimePeriodID %in% timeperiod_id,
                      .data$DatasetLength >= min_num_yrs) %>%
        dplyr::mutate(TaxonID = paste0('sp', .data$TaxonID), 
                      date = format(lubridate::date_decimal(.data$DecimalYearBegin), "%Y-%m-%d")) %>%
        dplyr::arrange(.data$date) %>%
        dplyr::select(-dplyr::one_of(c("SiblyFittedTheta", "SiblyThetaCILower", "SiblyThetaCIUpper", "SiblyExtremeNEffect",    
                                       "SiblyReturnRate", "SiblyCarryingCapacity", "Population", "Generation", 
                                       "SpatialDensity", "SpatialAccuracy")))
    
    abundance <- gpdd_data %>%
        dplyr::select(.data$date, .data$TaxonID, .data$PopulationUntransformed) %>%
        dplyr::group_by(.data$date, .data$TaxonID) %>%
        dplyr::summarize(total = sum(.data$PopulationUntransformed)) %>%
        tidyr::spread(key = .data$TaxonID, value = .data$total, fill = 0) %>%
        dplyr::ungroup() %>%
        dplyr::select(dplyr::starts_with('sp'))
    
    covariates <- gpdd_data %>% 
        dplyr::select(.data$date, .data$SampleYear, .data$DecimalYearBegin,
                      .data$DecimalYearEnd, .data$TaxonID, .data$PopulationUntransformed) %>%
        dplyr::group_by(.data$date, .data$TaxonID) %>%
        dplyr::summarize(total = sum(.data$PopulationUntransformed), 
                         SampleYear = mean(.data$SampleYear), 
                         DecimalYearBegin = min(.data$DecimalYearBegin), 
                         DecimalYearEnd = max(.data$DecimalYearEnd)) %>%
        tidyr::spread(key = .data$TaxonID, value = .data$total, fill = 0) %>%
        dplyr::ungroup() %>%
        dplyr::mutate_at(dplyr::vars(date), as.Date) %>% 
        dplyr::select(-dplyr::starts_with('sp'))
    
    location = gpdd_data %>% 
        dplyr::select(dplyr::one_of(c("BiotopeID", "LocationID", "ExactName", "TownName", "CountyStateProvince", 
                                      "Country", "Continent", "Ocean", "LongitudeMinutes", "EorW", 
                                      "LatitudeDegrees", "LatitudeMinutes", "NorS", "LongDD", "LatDD", 
                                      "North", "East", "South", "Area", "Notes.y", "LocationExtent"))) %>% 
        dplyr::distinct()
    
    samples = gpdd_data %>% 
        dplyr::select(dplyr::one_of(c("SamplingFrequency", "StartYear", "EndYear", "SamplingUnits", 
                                      "SamplingProtocol", "Reliability", "DatasetLength", "Notes.x", "Notes.y"))) %>% 
        dplyr::distinct()
    
    source = gpdd_data %>% 
        dplyr::select(.data$DataSourceID) %>% dplyr::distinct() %>%
        dplyr::left_join(rgpdd::gpdd_datasource, by = c("DataSourceID" = "DatasourceID"))
    
    metadata <- list(timename = "date", effort = NULL, location = location, 
                     samples = samples, source = source)
    
    return(list('abundance' = abundance, 'covariates' = covariates, "metadata" = metadata))
}
