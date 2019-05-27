#' @title Prepare BBS population time-series data
#' @description Modified from https://github.com/weecology/bbs-forecasting
#' and https://github.com/weecology/MATSS-community-change 
#' Selects sites with data spanning start_yr through end_yr containing at least min_num_yrs of data
#' samples during that period. Cleans data tables and stores each individual route as a .Rds file. Saves a data table of the route + region pairs. 
#' @param start_yr num first year of time-series
#' @param end_yr num last year of time-series
#' @param min_num_yrs num minimum number of years of data between start_yr & end_yr
#' @inheritParams get_mtquad_data
#' @return NULL
#' @export

prepare_bbs_ts_data <- function(start_yr = 1965, end_yr = 2017, min_num_yrs = 10,
                                path = get_default_data_path()){
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
        MATSS::filter_ts(start_yr, end_yr, min_num_yrs)
    
    
    bbs_routes_regions <- bbs_data %>%
        dplyr::select(bcr, route) %>%
        dplyr::distinct() %>%
        dplyr::mutate(name = paste0("bbs_bcr", bcr, "_route", route))
    
    if(!dir.exists(file.path(path, 'breed-bird-survey-prepped'))) {
        dir.create(file.path(path, 'breed-bird-survey-prepped'))
    }
    
    write.csv(bbs_routes_regions, file.path(path, "breed-bird-survey-prepped", "routes_and_regions_table.csv"), row.names = F)
    
    make_list <- function(bbs_line) {
        return(list(route = as.numeric(bbs_line[2]), region = as.numeric(bbs_line[1])))
    }
    bbs_routes_regions_list = apply(bbs_routes_regions, MARGIN = 1, FUN = make_list)
    
    lapply(bbs_routes_regions_list, FUN = subset_bbs_route_region_data, bbs_data_table = bbs_data, species_table = bbs_data_tables$breed_bird_survey_species, path = path)
    
}


#' Subset BBS data by route and reigon
#' Writes each route & region data object (a list of abundance, metadata, and covariates) as an .Rds file to be re-read via readRDS. 
#' @param route_region named list of route and region to subset to
#' @param bbs_data_table main bbs data table
#' @param species_table table of species for BBS
#' @param path path
#' @return nothing
#' @export
subset_bbs_route_region_data <- function(route_region, bbs_data_table, species_table,
                                         path = get_default_data_path()) {
    
    route = as.numeric(route_region$route)
    region = as.numeric(route_region$region)
    
    this_bbs_data <- bbs_data_table %>%
        dplyr::filter(bcr == region, route == route) %>%
        combine_subspecies(species_table = species_table) %>%
        filter_bbs_species(species_table = species_table) %>%
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
    
    metadata <- list(timename = 'year', effort = 'effort', route = route, region = region)
    
    
    if(!dir.exists(file.path(path, 'breed-bird-survey-prepped'))) {
        dir.create(file.path(path, 'breed-bird-survey-prepped'))
    }
    
    storage_path = file.path(path, 'breed-bird-survey-prepped')
    
    this_bbs_result = list('abundance' = abundance, 'covariates' = covariates, 'metadata' = metadata)
    
    saveRDS(this_bbs_result, file = file.path(storage_path, paste0("route", route, "region", region, ".Rds")) )
    
    return()
    
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
#' @export
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


#' Filter poorly sampled BBS species
#' 
#' Modified from https://github.com/weecology/bbs-forecasting/blob/master/R/forecast-bbs-core.R
#'
#' Removes waterbirds, shorebirds, owls, kingfishers, knightjars,
#' dippers. These species are poorly sampled due to their aquatic or
#' noctural nature. Also removes taxa that were either partially unidentified
#' (e.g. "sp.") or were considered hybrids (e.g. "A x B") or were listed as more
#' than one species (e.g. "A / B")
#'
#' @param df dataframe containing an species_id column
#'
#' @return dataframe, filtered version of initial dataframe
#' @export

filter_bbs_species <- function(df, species_table){
    
    is_unidentified = function(names) {
        #Before filtering, account for this one hybrid of 2 subspecies so it's kept
        names[names=='auratus auratus x auratus cafer']='auratus auratus'
        grepl('sp\\.| x |\\/', names)
    }
    
    valid_taxa = species_table %>%
        dplyr:: filter(!is_unidentified(species)) %>%
        dplyr::filter(aou > 2880) %>%
        dplyr::filter(aou < 3650 | aou > 3810) %>%
        dplyr::filter(aou < 3900 | aou > 3910) %>%
        dplyr::filter(aou < 4160 | aou > 4210) %>%
        dplyr::filter(aou != 7010)
    
    dplyr::filter(df, species_id %in% valid_taxa$aou)
}

#' Combine subspecies into their common species
#' 
#' Modified from https://github.com/weecology/bbs-forecasting/blob/master/R/forecast-bbs-core.R 
#'
#' @export
combine_subspecies = function(df, species_table){
    
    # Subspecies have two spaces separated by non-spaces
    subspecies_names = species_table %>%
        dplyr::filter(aou %in% unique(df$species_id)) %>%
        dplyr::pull(spanish_common_name) %>%
        grep(" [^ ]+ ", ., value = TRUE)
    
    subspecies_ids = species_table %>%
        dplyr::filter(spanish_common_name %in% subspecies_names) %>%
        dplyr::pull(aou)
    
    # Drop all but the first two words to get the root species name,
    # then find the AOU code
    new_subspecies_ids = species_table %>%
        dplyr::slice(match(stringr::word(subspecies_names, 1,2),
                           species_table$spanish_common_name)) %>%
        dplyr::pull(aou)
    
    # replace the full subspecies names with species-level names
    for (i in seq_along(subspecies_ids)) {
        df$species_id[df$species_id == subspecies_ids[i]] = new_subspecies_ids[i]
    }
    
    df_grouped <- df %>%
        dplyr::group_by_at(dplyr::vars(-abundance)) %>%
        dplyr::summarise(abundance = sum(abundance)) %>%
        dplyr::ungroup() %>%
        dplyr::distinct()
}
