#' @title Get cleaned BBS data
#' @description Gets prepped BBS data (as a list of abundance, covariates, and 
#'   metadata) for a specified route and region. First run `prepare_bbs_data` 
#'   to create these files from the raw BBS data tables. If the files are not 
#'   found, then `NULL` is returned.
#' @param route Route number
#' @param region Region number
#' @inheritParams get_mtquad_data
#' @return list of abundance, covariates, and metadata
#' @export
get_bbs_route_region_data = function(path = file.path(get_default_data_path(), "breed-bird-survey-prepped", 
                                                      paste0("route", route, "region", region, ".Rds")), 
                                     route = 1, 
                                     region = 11)
{
    if (file.exists(path)) {
        return(readRDS(path)) 
    } else {
        return(NULL)
    }
}

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
#' @param data_subset optional, a subset of the BBS communities to use 
#'   (to speed up development). As c(1:X)
#' @inheritParams get_mtquad_data
#' @return NULL
#' @export
prepare_bbs_ts_data <- function(start_yr = 1965, end_yr = 2017, min_num_yrs = 10,
                                path = get_default_data_path(), data_subset = NULL) 
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
    
    storage_path <- file.path(path, "breed-bird-survey-prepped")
    if (!dir.exists(storage_path)) {
        dir.create(storage_path)
    }
    
    utils::write.csv(bbs_routes_regions, 
                     file.path(storage_path, "routes_and_regions_table.csv"), 
                     row.names = F)
    
    # filter and process selected route and region combinations
    if (!is.null(data_subset)) {
        bbs_routes_regions <- bbs_routes_regions[data_subset, ]
    }
    
    bbs_routes_regions %>%
        dplyr::select(.data$bcr, .data$route) %>%
        purrr::pmap(function(bcr, route) {
            bbs_data %>%
                dplyr::filter(.data$bcr == !!bcr,
                              .data$route == !!route) %>%
                process_bbs_route_region_data(species_table = bbs_data_tables$breed_bird_survey_species) %>%
                saveRDS(file = file.path(storage_path, paste0("route", route, "region", bcr, ".Rds")) )
        })
    
    citation_from <- file.path(path, "breed-bird-survey", "CITATION")
    citation_to <- file.path(storage_path, "CITATION")
    file.copy(citation_from, citation_to)
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
        combine_bbs_subspecies(species_table = species_table) %>%
        filter_bbs_species(species_table = species_table) %>%
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
        dplyr::arrange(.data$year)
    
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
        dplyr::filter(.data$year >= start_yr, .data$year <= end_yr) %>%
        dplyr::group_by(.data$site_id) %>%
        dplyr::summarize(num_years = length(unique(.data$year))) %>%
        dplyr::ungroup() %>%
        dplyr::filter(.data$num_years >= min_num_yrs)
    
    filtered_data <- bbs_data %>%
        dplyr::filter(.data$year >= start_yr, .data$year <= end_yr) %>%
        dplyr::filter(.data$site_id %in% sites_to_keep$site_id)
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
        dplyr::filter(!is_unidentified(.data$species)) %>%
        dplyr::filter(.data$aou > 2880) %>%
        dplyr::filter(.data$aou < 3650 | .data$aou > 3810) %>%
        dplyr::filter(.data$aou < 3900 | .data$aou > 3910) %>%
        dplyr::filter(.data$aou < 4160 | .data$aou > 4210) %>%
        dplyr::filter(.data$aou != 7010)
    
    dplyr::filter(bbs_data_table, .data$species_id %in% valid_taxa$aou)
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
    subspecies_LUT <- species_table %>%
        dplyr::filter(.data$aou %in% unique(bbs_data_table$species_id)) %>%
        dplyr::filter(grepl(" [^ ]+ ", .data$spanish_common_name)) %>%
        dplyr::rename(old_id = .data$aou) %>%
        dplyr::mutate(species_name = stringr::word(.data$spanish_common_name, 1, 2)) %>%
        dplyr::left_join(dplyr::mutate_at(species_table, 
                                          "spanish_common_name", 
                                          as.character), 
                         by = c("species_name" = "spanish_common_name")) %>%
        dplyr::rename(new_id = .data$aou) %>%
        dplyr::mutate(new_id = ifelse(is.na(.data$new_id), .data$old_id, .data$new_id)) %>%
        dplyr::select_at(c("new_id", "old_id"))
    
    # replace the full subspecies names with species-level names
    if (NROW(subspecies_LUT) > 0)
    {
        new_levels <- subspecies_LUT$new_id
        names(new_levels) <- subspecies_LUT$old_id
        bbs_data_table$species_id <- dplyr::recode(bbs_data_table$species_id, !!!new_levels)
    }
    
    df_grouped <- bbs_data_table %>%
        dplyr::group_by_at(dplyr::vars(-.data$abundance)) %>%
        dplyr::summarize(abundance = sum(.data$abundance)) %>%
        dplyr::ungroup() %>%
        dplyr::distinct()
}
