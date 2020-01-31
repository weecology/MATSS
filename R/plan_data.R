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
        maizuru_data = get_maizuru_data(),
        jornada_data = get_jornada_data(),
        sgs_data = get_sgs_data(), 
        cowley_lizards_data = get_cowley_lizards(), 
        cowley_snakes_data = get_cowley_snakes(), 
        karoo_data = get_karoo_data(), 
        kruger_data = get_kruger_data()
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
        portal_data = get_portal_rodents(),
        sdl_data = drake::target(get_sdl_data(path = !!file.path(path, "veg-plots-sdl")),
                                 trigger = trigger(command = FALSE)), 
        mtquad_data = drake::target(get_mtquad_data(path = !!file.path(path, "mapped-plant-quads-mt")), 
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
    routes_and_regions <- utils::read.csv(routes_and_regions_file, colClasses = "character")
    
    # filter datasets and generate plan
    if (!is.null(data_subset)) {
        routes_and_regions <- routes_and_regions[data_subset, ]
    }
    bbs_datasets <- drake::drake_plan(
        bbs_data_rtrg = drake::target(get_bbs_route_region_data(path = file_in(!!file.path(path, "breed-bird-survey-prepped", 
                                                                                           paste0("route", route, "region", region, ".Rds")))),
                                      transform = map(route = !!rlang::syms(routes_and_regions$route),
                                                      region = !!rlang::syms(routes_and_regions$bcr)), 
                                      trigger = trigger(command = FALSE)
        )
    )
    return(bbs_datasets)
}

#' @title Generate a Drake Plan for GPDD Datasets
#' 
#' @inheritParams build_datasets_plan
#' @inheritParams get_gpdd_data
#' 
#' @return a drake plan (i.e. a tibble) specifying the targets and commands 
#'   for gathering GPDD datasets
#' 
#' @export
#' 
build_gpdd_datasets_plan <- function()
{
    locations_file <- system.file("extdata", "gpdd_locations.csv", 
                                  package = "MATSS", mustWork = TRUE)
    
    locations <- utils::read.csv(locations_file, colClasses = "character")
    
    gpdd_datasets <- drake::drake_plan(
        gpdd_data_rtrg = drake::target(get_gpdd_data(location_id = location_id, timeperiod_id = timeperiod_id),
                                       transform = map(location_id = !!rlang::syms(locations$LocationID),
                                                       timeperiod_id = !!rlang::syms(locations$TimePeriodID))
        )
    )
    return(gpdd_datasets)
}

#' @title Generate a Drake Plan for Biotime Datasets
#' 
#' @inheritParams build_datasets_plan
#' @param data_subset optional, a subset of the Biotime study_ids to use 
#'   (to speed up development). As c(1:X)
#' @param do_processing whether to process the datasets if necessary
#' 
#' @return a drake plan (i.e. a tibble) specifying the targets and commands 
#'   for gathering Biotime datasets
#' 
#' @export
#' 
build_biotime_datasets_plan <- function(path = get_default_data_path(), 
                                        data_subset = NULL, 
                                        do_processing = TRUE)
{
    dataset_ids <- get_biotime_dataset_ids(path = path, 
                                           data_subset = data_subset, 
                                           do_processing = do_processing)
    
    biotime_datasets <- drake::drake_plan(
        biotime_data_rtrg = drake::target(get_biotime_data(path = file_in(!!file.path(path, "biotime-prepped", 
                                                                                      paste0("dataset", dataset, ".Rds")))), 
                                          transform = map(dataset = !!rlang::syms(dataset_ids)), 
                                          trigger = trigger(command = FALSE)
        )
    )
    return(biotime_datasets)
}
