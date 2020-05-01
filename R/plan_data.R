#' @title Generate a Drake Plan for Datasets
#' 
#' @param path where to get the downloaded retriever datasets
#' @param include_retriever_data whether to include retriever-downloaded data
#' @param include_bbs_data whether to include BBS data
#' @param bbs_subset optional, a subset of the BBS communities to use 
#'   (to speed up development). As c(1:X)
#' @param include_gpdd_data whether to include gpdd data
#' @param include_biotime_data whether to include biotime data
#' @param biotime_subset optional, a subset of the biotime study_ids to use 
#'   (to speed up development). As c(1:X)
#' @param biotime_process whether to process the biotime datasets when building 
#'   the plan
#' 
#' @return a drake plan (i.e. a tibble) specifying the targets and commands 
#'   for gathering datasets
#' 
#' @export
#' 
build_datasets_plan <- function(path = get_default_data_path(), 
                                include_retriever_data = FALSE,
                                include_bbs_data = FALSE,
                                bbs_subset = NULL,
                                include_gpdd_data = FALSE, 
                                include_biotime_data = FALSE, 
                                biotime_subset = NULL, 
                                biotime_process = TRUE)
{
    datasets <- drake::drake_plan(
        cowley_lizards = get_cowley_lizards(), 
        cowley_snakes = get_cowley_snakes(), 
        karoo = get_karoo_data(), 
        kruger = get_kruger_data(), 
        portal_rodents = get_portal_rodents()
    )
    
    if (include_retriever_data)
    {
        datasets <- dplyr::bind_rows(datasets, 
                                     build_retriever_datasets_plan(path = path))
    }
    
    if (include_bbs_data)
    {
        datasets <- dplyr::bind_rows(datasets, 
                                     build_bbs_datasets_plan(path = path, data_subset = bbs_subset))
    }
    
    if (include_gpdd_data)
    {
        datasets <- dplyr::bind_rows(datasets, 
                                     build_gpdd_datasets_plan())
    }
    
    if (include_biotime_data)
    {
        datasets <- dplyr::bind_rows(datasets, 
                                     build_biotime_datasets_plan(path = path, 
                                                                 data_subset = biotime_subset, 
                                                                 do_processing = biotime_process))
    }
    return(datasets)
}

#' @title Generate a Drake Plan for retriever datasets
#' 
#' @inheritParams build_datasets_plan
#' 
#' @return a drake plan (i.e. a tibble) specifying the targets and commands 
#'   for retriever downloaded datasets
#' 
#' @export
#' 
build_retriever_datasets_plan <- function(path = get_default_data_path())
{
    drake::drake_plan(
        maizuru_fish = drake::target(get_maizuru_data(path = !!file.path(path, "ushio-maizuru-fish-community")),
                                     trigger = trigger(command = FALSE)), 
        jornada_rodents = drake::target(get_jornada_data(path = !!file.path(path, "jornada-lter-rodent")),
                                        trigger = trigger(command = FALSE)), 
        shortgrass_steppe = drake::target(get_sgs_data(path = !!file.path(path, "shortgrass-steppe-lter")),
                                          trigger = trigger(command = FALSE)), 
        sonoran_desert_lab = drake::target(get_sdl_data(path = !!file.path(path, "veg-plots-sdl")),
                                           trigger = trigger(command = FALSE)), 
        montana_plantquads = drake::target(get_mtquad_data(path = !!file.path(path, "mapped-plant-quads-mt")), 
                                           trigger = trigger(command = FALSE))
    )
}


#' @title Generate a Drake Plan for BBS Datasets
#' 
#' @inheritParams build_datasets_plan
#' @inheritParams prepare_bbs_ts_data
#' 
#' @return a drake plan (i.e. a tibble) specifying the targets and commands 
#'   for gathering BBS datasets
#' 
#' @export
#' 
build_bbs_datasets_plan <- function(path = get_default_data_path(), data_subset = NULL)
{
    # get metadata on routes and regions
    routes_and_regions_file <- file.path(path, "breed-bird-survey-prepped", 
                                         "routes_and_regions_table.csv")
    if (!file.exists(routes_and_regions_file)) {
        message("preprocessing bbs timeseries data")
        prepare_bbs_ts_data(path = path, data_subset = data_subset)
    }
    routes_and_regions <- utils::read.csv(routes_and_regions_file, 
                                          colClasses = "character")
    
    # filter datasets and generate plan
    if (!is.null(data_subset)) {
        routes_and_regions <- routes_and_regions[data_subset, ]
    }
    bbs_datasets <- drake::drake_plan(
        bbs_rtrg = drake::target(get_bbs_route_region_data(path = !!file.path(path, "breed-bird-survey-prepped", 
                                                                              paste0("route", route, "region", region, ".RDS"))),
                                 transform = map(route = !!routes_and_regions$route,
                                                 region = !!routes_and_regions$statenum), 
                                 trigger = trigger(command = FALSE)
        )
    )
    return(bbs_datasets)
}

#' @title Generate a Drake Plan for GPDD Datasets
#' 
#' @return a drake plan (i.e. a tibble) specifying the targets and commands 
#'   for gathering GPDD datasets
#' 
#' @export
#' 
build_gpdd_datasets_plan <- function(path = get_deault_data_path(), data_subset = NULL)
{
    processed_locations_file <- file.path(path, "gpdd-prepped", 
                                          "gpdd_locations.csv")
    
    if (!file.exists(processed_locations_file)) {
        message("preprocessing gpdd timeseries data")
        prepare_gpdd_data(path = path, data_subset = data_subset)
    }
    processed_locations <- utils::read.csv(processed_locations_file, 
                                           colClasses = "character")
    
    gpdd_datasets <- drake::drake_plan(
        gpdd_rtrg = drake::target(get_gpdd_data(location_id = location_id),
                                  transform = map(location_id = !!processed_locations$location_id), 
                                  trigger = trigger(command = FALSE)
        )
    )
    return(gpdd_datasets)
}


build_bbs_datasets_plan <- function(path = get_default_data_path(), data_subset = NULL)
{
    # get metadata on routes and regions
    routes_and_regions_file <- file.path(path, "breed-bird-survey-prepped", 
                                         "routes_and_regions_table.csv")
    if (!file.exists(routes_and_regions_file)) {
        message("preprocessing bbs timeseries data")
        prepare_bbs_ts_data(path = path, data_subset = data_subset)
    }
    routes_and_regions <- utils::read.csv(routes_and_regions_file, 
                                          colClasses = "character")
    
    # filter datasets and generate plan
    if (!is.null(data_subset)) {
        routes_and_regions <- routes_and_regions[data_subset, ]
    }
    bbs_datasets <- drake::drake_plan(
        bbs_rtrg = drake::target(get_bbs_route_region_data(path = !!file.path(path, "breed-bird-survey-prepped", 
                                                                              paste0("route", route, "region", region, ".RDS"))),
                                 transform = map(route = !!routes_and_regions$route,
                                                 region = !!routes_and_regions$statenum), 
                                 trigger = trigger(command = FALSE)
        )
    )
    return(bbs_datasets)
}


#' @title Generate a Drake Plan for Biotime Datasets
#' 
#' @inheritParams build_datasets_plan
#' @param data_subset optional, a subset of the Biotime study_ids to use 
#'   (to speed up development). As c(1:X)
#' @param do_processing whether to process the datasets if necessary
#' @param force_reprocessing whether to force re-processing of datasets
#' 
#' @return a drake plan (i.e. a tibble) specifying the targets and commands 
#'   for gathering Biotime datasets
#' 
#' @export
#' 
build_biotime_datasets_plan <- function(path = get_default_data_path(), 
                                        data_subset = NULL, 
                                        do_processing = TRUE, 
                                        force_reprocessing = FALSE)
{
    dataset_ids <- get_biotime_dataset_ids(path = path, 
                                           data_subset = data_subset, 
                                           do_processing = do_processing, 
                                           force_reprocessing = force_reprocessing)
    
    biotime_datasets <- drake::drake_plan(
        biotime_rtrg = drake::target(get_biotime_data(path = !!file.path(path, "biotime-prepped", 
                                                                         paste0("dataset", dataset, ".RDS"))), 
                                     transform = map(dataset = !!dataset_ids), 
                                     trigger = trigger(command = FALSE)
        )
    )
    return(biotime_datasets)
}
