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
#' @examples
#' \dontrun{
#'   get_bbs_data(bbs_data_tables=retriever_data()$'breed-bird-survey',region=7)
#' }
#' @export

get_bbs_data <- function(bbs_data_tables, start_yr=1965, end_yr=2017, min_num_yrs=10, region)
{
    bbs_data <- bbs_data_tables$breed_bird_survey_weather %>%
        dplyr::filter(runtype==1, rpid==101) %>%
        dplyr::left_join(bbs_data_tables$breed_bird_survey_counts, 
                         by = c('statenum','route','rpid','year','routedataid','countrynum')) %>%
        dplyr::left_join(bbs_data_tables$breed_bird_survey_routes, 
                         by = c('statenum','route','countrynum')) %>%
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
        dplyr::summarise(num_years = length(unique(year))) %>%
        dplyr::ungroup() %>%
        dplyr::filter(num_years >= min_num_yrs)
    
    filtered_data <- bbs_data %>%
        dplyr::filter(year >= start_yr, year <= end_yr) %>%
        dplyr::filter(site_id %in% sites_to_keep$site_id)
}

#' @title Create Sonoran desert lab time-series data
#'
#' @description Original data found here http://www.eebweb.arizona.edu/faculty/venable/LTREB/LTREB%20data.htm
#'
#' @param sdl_data_tables list of all SDL tables (Count1906,Photo_info,Plot_corners,
#' Plots,Seedling_counts,SMCover,SMDensity,Species,Stake_info)
#' @param plots vector of plots to keep
#'
#' @return list of two dataframes, one with abundance data, the other with covariate data
#' 
#' @examples
#' \dontrun{
#'   get_sdl_data(sdl_data_tables=retriever_data()$'veg-plots-sdl')
#' }
#' @export

get_sdl_data <- function(sdl_data_tables, plots = c(4,7,8,9,10,11,12,14,15,16,17)){
    sdl_data <- sdl_data_tables$veg_plots_sdl_SMDensity %>%
        dplyr::select(-countns) %>%
        dplyr::filter(plot %in% plots) %>%
        dplyr::group_by(year,code) %>%
        dplyr::summarise(count = sum(count)) %>%
        tidyr::spread(key = code, value = count, fill = 0) %>%
        dplyr::rename(UNKN=V1) %>%
        dplyr::ungroup()
    
    abundance <- sdl_data %>%
        dplyr::select(-year)
    
    covariates <- sdl_data %>%
        dplyr::select(year)
    
    return(list('abundance' = abundance, 'covariates' = covariates))
}

#' @title Create Montana plant quad time-series data
#'
#' @description Original data found here 
#'
#' @param mtquad_data_tables list of all montana plant tables (allrecords_cover,
#' allrecords_density,species_list)
#'
#' @return list of two dataframes, one with abundance data, the other with covariate data
#' 
#' @examples
#' \dontrun{
#'   get_mtquad_data(mtquad_data_tables=retriever_data()$'mapped-plant-quads-mt')
#' }
#' @export

get_mtquad_data <- function(mtquad_data_tables){
    mtquad_data <- mtquad_data_tables$mapped_plant_quads_mt_allrecords_density %>%
        dplyr::select(-objectid,-seedling,-x,-y) %>%
        dplyr::group_by(year,species,quad) %>%
        dplyr::summarise(abundance = sum(stems)) %>%
        dplyr::group_by(year,species) %>%
        dplyr::summarise(abundance = sum(abundance)) %>%
        tidyr::spread(key = species, value = abundance, fill = 0) %>%
        dplyr::ungroup()
    
    abundance <- mtquad_data %>%
        dplyr::select(-year)
    
    covariates <- mtquad_data %>%
        dplyr::select(year)
    
    return(list('abundance' = abundance, 'covariates' = covariates))
}
