#' @title Create BBS population time-series data
#'
#' @description Modified from https://github.com/weecology/bbs-forecasting
#' and https://github.com/weecology/MATSS-community-change 
#' Selects sites with data spanning start_yr through end_yr containing at least min_num_yrs of data
#' samples during that period.
#' @param start_yr num first year of time-series
#' @param end_yr num last year of time-series
#' @param min_num_yrs num minimum number of years of data between start_yr & end_yr
#' @inheritParams get_mtquad_data
#'
#' @return list of three dataframes (one with bbs data filtered to time series that meet the criteria, one with the BBS species table, and one with the routes and regions represented in the first dataframe).
#' @export

get_bbs_ts_data <- function(start_yr = 1965, end_yr = 2017, min_num_yrs = 10,
                            path = get_default_data_path())
{
    bbs_data_tables <- import_retriever_data("breed-bird-survey", path = path)
    
    bbs_data <- bbs_data_tables$breed_bird_survey_weather %>%
        dplyr::filter(runtype == 1, rpid == 101) %>%
        dplyr::left_join(bbs_data_tables$breed_bird_survey_counts, 
                         by = c('statenum', 'route', 'rpid', 'year', 'routedataid', 'countrynum')) %>%
        dplyr::left_join(bbs_data_tables$breed_bird_survey_routes, 
                         by = c('statenum', 'route', 'countrynum')) %>%
        dplyr::mutate(site_id = statenum*1000 + route, 
                      starttemp = dplyr::case_when(tempscale=='F' ~ c((starttemp - 32)*5/9),
                                                   tempscale=='C' ~ as.double(starttemp)),
                      endtemp = dplyr::case_when(tempscale=='F' ~ c((endtemp - 32)*5/9),
                                                 tempscale=='C' ~ as.double(endtemp))) %>%
        dplyr::rename(lat = latitude,
                      long = longitude,
                      species_id = aou,
                      abundance = speciestotal) %>%
        filter_ts(start_yr, end_yr, min_num_yrs)
    
    
    bbs_routes_regions <- bbs_data %>%
        dplyr::select(bcr, route) %>%
        dplyr::distinct() %>%
        dplyr::mutate(bcr = as.character(bcr), route = as.character(route)) %>%
        dplyr::mutate(name = paste0("bbs_bcr", bcr, "_route", route))
    
    bbs_ts_data = list(bbs_data = bbs_data, species_table = bbs_data_tables$breed_bird_survey_species, routes_and_regions = bbs_routes_regions)
    
    return(bbs_ts_data)
}


#' Get BBS data by route and reigon
#'
#' @param route route
#' @param region region
#' @param bbs_ts_data list of three dataframes (output of get_bbs_ts_data)
#' @return list of two dataframes (one with abundance data, the other with covariate data) 
#'   and one list of metadata.
#' @export
get_bbs_route_region_data <- function(route, region, bbs_ts_data) {

    route = as.numeric(route)
    region = as.numeric(region)
    
    this_bbs_data <- bbs_ts_data$bbs_data %>%
        dplyr::filter(bcr == region, route == route) %>%
        combine_subspecies(species_table = bbs_ts_data$species_table) %>%
        filter_bbs_species(species_table = bbs_ts_data$species_table) %>%
        dplyr::mutate(species_id = paste('sp', species_id, sep=''),
                      date = as.Date(paste(year, month, day, sep = "-"))) %>%
        dplyr::ungroup() 
    
    abundance <- this_bbs_data %>%
        dplyr::group_by(year, species_id) %>%
        dplyr::summarise(abundance = sum(abundance)) %>%
        dplyr::ungroup() %>%
        tidyr::spread(key = species_id, value = abundance, fill = 0) %>%
        dplyr::arrange(year) %>%
        dplyr::select(-year)
    
    covariates <- this_bbs_data %>%
        dplyr::group_by(year) %>%
        dplyr::summarise(effort = dplyr::n_distinct(site_id),
                         starttemp = mean(starttemp), endtemp = mean(endtemp),
                         startwind = mean(startwind), endwind = mean(endwind),
                         startsky = mean(startsky), endsky = mean(endsky),
                         lat = mean(lat), long = mean(long), mean_date = mean(date)) %>%
        dplyr::arrange(year)
    
    metadata <- list(timename = 'year', effort = 'effort', route = this_route, region = this_bcr)
    
    return(list('abundance' = abundance, 'covariates' = covariates, 'metadata' = metadata))
    
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
#' @param plots vector of plots to keep
#' @inheritParams get_mtquad_data
#'
#' @return list of two dataframes (one with abundance data, the other with covariate data) 
#'   and one list of metadata.
#'
#' @examples
#' \dontrun{
#'   get_sdl_data(sdl_data_tables=retriever_data()$'veg-plots-sdl')
#' }
#' @export

get_sdl_data <- function(plots = c(4, 7, 8, 9, 10, 11, 12, 14, 15, 16, 17), 
                         path = get_default_data_path())
{
    sdl_data_tables <- import_retriever_data("veg-plots-sdl", path = path)
    
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
    
    metadata <- list(timename = "year", effort = NULL)
    return(list('abundance' = abundance, 'covariates' = covariates, 
                "metadata" = metadata))
}

#' @title Create Montana plant quad time-series data
#'
#' @description Original data found here 
#'
#' @param path where to load the raw data files from
#'
#' @return list of two dataframes (one with abundance data, the other with covariate data) 
#'   and one list of metadata.
#' 
#' @examples
#' \dontrun{
#'   get_mtquad_data(mtquad_data_tables=retriever_data()$'mapped-plant-quads-mt')
#' }
#' @export

get_mtquad_data <- function(path = get_default_data_path())
{
    mtquad_data_tables <- import_retriever_data('mapped-plant-quads-mt', path = path)
    
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
    
    metadata <- list(timename = "year", effort = NULL)
    return(list('abundance' = abundance, 'covariates' = covariates, 
                "metadata" = metadata))
}
