#' @title Create GPDD population time-series data
#'
#' @description Selects sites containing at 
#' least min_num_yrs of data samples during that period.
#'
#' @param min_num_yrs minimum number of years of data
#' @param locationid Location code of data to return
#'
#' @return list of two dataframes (one with abundance data, the other with covariate data) 
#'   and one list of metadata.
#' 
#' @examples
#' \dontrun{
#'   get_gpdd_data(locationid=83, timeperiodid=1)
#' }
#' @export

get_gpdd_data <- function(min_num_yrs = 10, locationid, timeperiodid)
{
    
gpdd_data <- rgpdd::gpdd_data %>% 
             dplyr::left_join(rgpdd::gpdd_main, by = "MainID") %>%
             dplyr::left_join(rgpdd::gpdd_location, by = "LocationID") %>%
             dplyr::filter(LocationID==locationid, TimePeriodID == timeperiodid,
                           DatasetLength >= min_num_yrs) %>%
             dplyr::mutate(TaxonID = paste('sp', TaxonID, sep=''), 
                           date = format(lubridate::date_decimal(DecimalYearBegin), "%d-%m-%Y")) %>%
             dplyr::arrange(date) %>%
             dplyr::select(-c(SiblyFittedTheta, SiblyThetaCILower, SiblyThetaCIUpper, SiblyExtremeNEffect,    
                              SiblyReturnRate, SiblyCarryingCapacity, Population, Generation, 
                              SpatialDensity, SpatialAccuracy))

abundance <- gpdd_data %>%
    dplyr::select(date,TaxonID,PopulationUntransformed) %>%
    dplyr::group_by(date,TaxonID) %>%
    dplyr::summarise(total = sum(PopulationUntransformed)) %>%
    tidyr::spread(key = TaxonID, value = total, fill = 0) %>%
    dplyr::ungroup() %>%
    dplyr::select(dplyr::starts_with('sp'))

covariates <- gpdd_data %>% 
    dplyr::select(date, SampleYear, DecimalYearBegin,
                  DecimalYearEnd, TaxonID, PopulationUntransformed) %>%
    dplyr::group_by(date, TaxonID) %>%
    dplyr::summarise(total = sum(PopulationUntransformed), SampleYear = mean(SampleYear), 
                     DecimalYearBegin = min(DecimalYearBegin), DecimalYearEnd = max(DecimalYearEnd)) %>%
    tidyr::spread(key = TaxonID, value = total, fill = 0) %>%
    dplyr::ungroup() %>%
    dplyr::select(-dplyr::starts_with('sp'))

location = gpdd_data %>% 
    dplyr::select(BiotopeID, LocationID, ExactName, TownName, CountyStateProvince, Country, 
                  Continent, Ocean, LongitudeMinutes, EorW, LatitudeDegrees, LatitudeMinutes, 
                  NorS, LongDD, LatDD, North, East, South, Area, 
                  Notes.y, LocationExtent) %>% dplyr::distinct()

samples = gpdd_data %>% 
    dplyr::select(SamplingFrequency, 
                  StartYear, EndYear, SamplingUnits, SamplingProtocol, 
                  Reliability, DatasetLength, Notes.x, Notes.y) %>% dplyr::distinct()

source = gpdd_data %>% 
    dplyr::select(DataID, MainID, DataSourceID, SourceDimension, 
                  SourceTransformReference, AssociatedDataSets) %>% dplyr::distinct()

metadata <- list(timename = "date", effort=NULL, location = location, samples = samples, source = source)

return(list('abundance' = abundance, 'covariates' = covariates, "metadata" = metadata))
    
}
    