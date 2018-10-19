#' @title Create BBS population time-series data
#'
#' @description Modified from https://github.com/weecology/bbs-forecasting
#' and https://github.com/weecology/MATSS-community-change 
#' Selects sites with data spanning start_yr through end_yr containing at least min_num_yrs of data
#' samples during that period.
#'
#' @param bbs_data_tables list of all BBS tables (counts,region_codes,routes,species,weather)
#' @param start_yr num first year of time-series
#' @param end_yr num last year of time-series
#' @param min_num_yrs num minimum number of years of data between start_yr & end_yr
#' @param region region code of data to return (currently uses state codes)
#'
#' @return list of two dataframes, one with abundance data, the other with covariate data
#' 
#' @examples get_bbs_data(bbs_data_tables=retriever_data()$'breed-bird-survey',region=7)
#' @export

get_bbs_data <- function(bbs_data_tables, start_yr=1965, end_yr=2017, min_num_yrs=10, region){
    bbs_data <- bbs_data_tables$weather %>%
        dplyr::filter(runtype==1, rpid==101) %>%
        dplyr::left_join(bbs_data_tables$counts, by = c('statenum','route','rpid','year','routedataid','countrynum')) %>%
        dplyr::left_join(bbs_data_tables$routes, by = c('statenum','route','countrynum')) %>%
        dplyr::filter(statenum==region) %>%
        dplyr::mutate(site_id = statenum*1000 + route) %>%
        dplyr::rename(lat = latitude,
                      long = longitude,
                      species_id = aou,
                      abundance = speciestotal) %>%
        dplyr::mutate(species_id = paste('sp',species_id,sep='')) %>%
        filter_ts(start_yr, end_yr, min_num_yrs) %>%
        dplyr::ungroup() %>%
        dplyr::select(-rpid,-runtype,-count10,-count20,-count30,-count40,-count50) %>%
        tidyr::spread(key = species_id, value = abundance, fill = 0)
    
    abundance <- bbs_data %>%
        dplyr::select(dplyr::starts_with('sp'))
    
    covariates <- bbs_data %>%
        dplyr::select(-dplyr::starts_with('sp'))
    
    return(list('abundance' = abundance, 'covariates' = covariates))
}

#' @title Filter BBS to specified time series period and number of samples
#'
#' @description Modified from https://github.com/weecology/bbs-forecasting 
#' and https://github.com/weecology/MATSS-community-change
#'
#' @param bbs_data dataframe that contains BBS site_id and year columns
#' @param start_yr num first year of time-series
#' @param end_yr num last year of time-series
#' @param min_num_yrs num minimum number of years of data between start_yr & end_yr
#'
#' @return dataframe with original data and associated environmental data
filter_ts <- function(bbs_data, start_yr, end_yr, min_num_yrs) {
    sites_to_keep = bbs_data %>%
        dplyr::filter(year >= start_yr, year <= end_yr) %>%
        dplyr::group_by(site_id) %>%
        dplyr::summarise(num_years=length(unique(year))) %>%
        dplyr::ungroup() %>%
        dplyr::filter(num_years >= min_num_yrs)
    
    filtered_data <- bbs_data %>%
        dplyr::filter(year >= start_yr, year <= end_yr) %>%
        dplyr::filter(site_id %in% sites_to_keep$site_id)
}
