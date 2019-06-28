#' @title Prepare BBS population time-series data
#' @description Modified from \url{https://github.com/weecology/bbs-forecasting}
#'   and \url{https://github.com/weecology/MATSS-community-change}. 
#' 
#' Selects sites with data spanning `start_yr` through `end_yr` containing at 
#'   least `min_num_yrs` of data samples during that period. Cleans data tables 
#'   and stores each individual route as a .Rds file. Saves a data table of the 
#'   route + region pairs. 
#' @param start_yr num first year of time-series
#' @param end_yr num last year of time-series
#' @param min_num_yrs num minimum number of years of data between start_yr & end_yr
#' @param bbs_subset optional, a subset of the BBS communities to use 
#'   (to speed up development). As c(1:X)
#' @inheritParams get_mtquad_data
#' @return NULL
#' @export

prepare_bbs_ts_data <- function(start_yr = 1965, end_yr = 2017, min_num_yrs = 10,
                                path = get_default_data_path(), bbs_subset = NULL) 
{
    bbs_data_tables <- import_retriever_data("breed-bird-survey", path = path)
    
    bbs_data <- bbs_data_tables$breed_bird_survey_weather %>%
        dplyr::filter(.data$runtype == 1, .data$rpid == 101) %>%
        dplyr::left_join(bbs_data_tables$breed_bird_survey_counts,
                         by = c('statenum', 'route', 'rpid', 'year', 'routedataid', 'countrynum')) %>%
        dplyr::left_join(bbs_data_tables$breed_bird_survey_routes, 
                         by = c('statenum', 'route', 'countrynum')) %>%
        dplyr::mutate(site_id = .data$statenum*1000 + .data$route, 
                      starttemp = dplyr::case_when(.data$tempscale == 'F' ~ c((.data$starttemp - 32)*5/9),
                                                   .data$tempscale == 'C' ~ as.double(.data$starttemp)),
                      endtemp = dplyr::case_when(.data$tempscale == 'F' ~ c((.data$endtemp - 32)*5/9),
                                                 .data$tempscale == 'C' ~ as.double(.data$endtemp))) %>%
        dplyr::rename(lat = .data$latitude,
                      long = .data$longitude,
                      species_id = .data$aou,
                      abundance = .data$speciestotal) %>%
        filter_bbs_ts(start_yr, end_yr, min_num_yrs)
    
    # prepare and write out route and region metadata
    bbs_routes_regions <- bbs_data %>%
        dplyr::select(.data$bcr, .data$route) %>%
        dplyr::distinct() %>%
        dplyr::mutate(name = paste0("bbs_bcr", .data$bcr, "_route", .data$route))
    
    storage_path <- file.path(path, 'breed-bird-survey-prepped')
    if (!dir.exists(storage_path)) {
        dir.create(storage_path)
    }
    
    utils::write.csv(bbs_routes_regions, 
                     file.path(storage_path, "routes_and_regions_table.csv"), 
                     row.names = F)
    
    # filter and process selected route and region combinations
    if (!is.null(bbs_subset)) {
        bbs_routes_regions <- bbs_routes_regions[bbs_subset, ]
    }
    
    bbs_routes_regions %>%
        dplyr::select(bcr, route) %>%
        purrr::pmap(function(bcr, route) {
            bbs_data %>%
                dplyr::filter(.data$bcr == !!bcr,
                              .data$route == !!route) %>%
                process_bbs_route_region_data(species_table = bbs_data_tables$breed_bird_survey_species) %>%
                saveRDS(file = file.path(storage_path, paste0("route", route, "region", bcr, ".Rds")) )
        })
}

#' @title Process the BBS data for an individual route and region
#' @description Correct and otherwise filter BBS species data (see 
#'   \code{\link{combine_bbs_subspecies}} and \code{\link{filter_bbs_species}} 
#'   for more info). Generate the abundance, covariate, and metadata tables and 
#'   return the combined object.
#' @param bbs_data_table main bbs data table
#' @param species_table table of species for BBS
#' @return the processed BBS data
#' @export
process_bbs_route_region_data <- function(bbs_data_table, species_table)
{
    # check that exactly one route and one region are represented in the data
    route <- unique(bbs_data_table$route)
    region <- unique(bbs_data_table$bcr)
    stopifnot(length(route) == 1 && 
                  length(region == 1))
    
    # process species IDs
    this_bbs_data <- bbs_data_table %>%
        combine_bbs_subspecies(species_table = .data$species_table) %>%
        filter_bbs_species(species_table = .data$species_table) %>%
        dplyr::mutate(species_id = paste0('sp', .data$species_id),
                      date = as.Date(paste(.data$year, .data$month, .data$day, sep = "-"))) %>%
        dplyr::ungroup() 
    
    abundance <- this_bbs_data %>%
        dplyr::group_by(.data$year, .data$species_id) %>%
        dplyr::summarize(abundance = sum(.data$abundance)) %>%
        dplyr::ungroup() %>%
        tidyr::spread(key = .data$species_id, value = .data$abundance, fill = 0) %>%
        dplyr::arrange(.data$year) %>%
        dplyr::select(-.data$year)
    
    covariates <- this_bbs_data %>%
        dplyr::group_by(.data$year) %>%
        dplyr::summarize(effort = dplyr::n_distinct(.data$site_id),
                         starttemp = mean(.data$starttemp), endtemp = mean(.data$endtemp),
                         startwind = mean(.data$startwind), endwind = mean(.data$endwind),
                         startsky = mean(.data$startsky), endsky = mean(.data$endsky),
                         lat = mean(.data$lat), long = mean(.data$long), 
                         mean_date = mean(.data$date)) %>%
        dplyr::arrange(year)
    
    metadata <- list(timename = 'year', effort = 'effort', route = route, region = region)
    
    return(list('abundance' = abundance, 'covariates' = covariates, 'metadata' = metadata))
}


#' @title Filter BBS to specified time series period and number of samples
#'
#' @description Modified from \url{https://github.com/weecology/bbs-forecasting} 
#' and \url{https://github.com/weecology/MATSS-community-change}
#'
#' @param bbs_data dataframe that contains BBS site_id and year columns
#' @param start_yr num first year of time-series
#' @param end_yr num last year of time-series
#' @param min_num_yrs num minimum number of years of data between start_yr & end_yr
#'
#' @return dataframe with original data and associated environmental data
#' @export
filter_bbs_ts <- function(bbs_data, start_yr, end_yr, min_num_yrs) {
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


#' @title Filter poorly sampled BBS species
#' 
#' @description Modified from \url{https://github.com/weecology/bbs-forecasting}
#'
#' Removes waterbirds, shorebirds, owls, kingfishers, knightjars,
#' dippers. These species are poorly sampled due to their aquatic or
#' noctural nature. Also removes taxa that were either partially unidentified
#' (e.g. "sp.") or were considered hybrids (e.g. "A x B") or were listed as more
#' than one species (e.g. "A / B")
#'
#' @inheritParams process_bbs_route_region_data
#'
#' @return dataframe, filtered version of initial dataframe
#' @export

filter_bbs_species <- function(bbs_data_table, species_table)
{
    is_unidentified <- function(names) {
        #Before filtering, account for this one hybrid of 2 subspecies so it's kept
        names[names == 'auratus auratus x auratus cafer'] = 'auratus auratus'
        grepl('sp\\.| x |\\/', names)
    }
    
    valid_taxa <- species_table %>%
        dplyr:: filter(!is_unidentified(species)) %>%
        dplyr::filter(aou > 2880) %>%
        dplyr::filter(aou < 3650 | aou > 3810) %>%
        dplyr::filter(aou < 3900 | aou > 3910) %>%
        dplyr::filter(aou < 4160 | aou > 4210) %>%
        dplyr::filter(aou != 7010)
    
    dplyr::filter(bbs_data_table, species_id %in% valid_taxa$aou)
}

#' @title Combine subspecies into their common species
#' 
#' @description Modified from \url{https://github.com/weecology/bbs-forecasting}
#' 
#' @inheritParams process_bbs_route_region_data
#' 
#' @export
combine_bbs_subspecies <- function(bbs_data_table, species_table) 
{
    # Subspecies have two spaces separated by non-spaces
    subspecies_names <- species_table %>%
        dplyr::filter(aou %in% unique(bbs_data_table$species_id)) %>%
        dplyr::pull(spanish_common_name) %>%
        grep(" [^ ]+ ", ., value = TRUE)
    
    subspecies_ids <- species_table %>%
        dplyr::filter(spanish_common_name %in% subspecies_names) %>%
        dplyr::pull(aou)
    
    # Drop all but the first two words to get the root species name,
    # then find the AOU code
    new_subspecies_ids <- species_table %>%
        dplyr::slice(match(stringr::word(subspecies_names, 1,2),
                           species_table$spanish_common_name)) %>%
        dplyr::pull(aou)
    
    # replace the full subspecies names with species-level names
    if (length(new_subspecies_ids) > 0)
    {
        names(new_subspecies_ids) <- subspecies_ids
        bbs_data_table$species_id <- dplyr::recode(bbs_data_table$species_id, !!!new_subspecies_ids)
    }
    
    df_grouped <- bbs_data_table %>%
        dplyr::group_by_at(dplyr::vars(-abundance)) %>%
        dplyr::summarise(abundance = sum(abundance)) %>%
        dplyr::ungroup() %>%
        dplyr::distinct()
}
