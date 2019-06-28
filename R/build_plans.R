#' @title Collect the Analyses Together as a List
#' 
#' @description This is a helper function that enables the NSE evaluation in 
#'   \code{\link{plan_analyses}}. For example, `collect_analyses(list(a, b))` 
#'   will return a list of two named elements, `a` and `b` corresponding to the 
#'   objects `a` and `b`. 
#' 
#' @param list_of_results the list of objects
#' 
#' @return a drake plan (i.e. a tibble) specifying the targets and commands 
#'   for all the analyses and the collected results (grouping the outputs from 
#'   each method into a single list)
#' 
#' @export
#' 
collect_analyses <- function(list_of_results)
{
    names(list_of_results) <- all.vars(match.call()$list_of_results)
    list_of_results
}

#' @title Generate a Drake Plan for Analyses
#' 
#' @description Given M methods to be applied to N datasets, make a drake plan 
#'   that contains an `analysis` targets corresponding to each M x N 
#'   combination, as well as M `results` targets corresponding to a list of 
#'   the `analysis` outputs for each of the M methods.
#' 
#' @param methods a drake plan listing the methods to be applied (it is 
#'   expected that each method is a function that takes in a dataset object)
#' @param datasets a drake plan listing the datasets to be analyzed
#' @param ... arguments to be passed to \code{drake::\link[drake]{drake_plan}} 
#' 
#' @return a drake plan (i.e. a tibble) specifying the targets and commands 
#'   for all the analyses and the collected results (grouping the outputs from 
#'   each method into a single list)
#' 
#' @export
#' 
build_analyses_plan <- function(methods, datasets, ...)
{
    ## The combination of each method x dataset
    drake::drake_plan(
        # expand out each `fun(data)``, where
        #   `fun` is each of the values in methods$target
        #   `data` is each of the values in datasets$target
        # note: tidyeval syntax is to get all the values from the previous plans,
        #       but keep them as unevaluated symbols, so that drake_plan handles
        #       them appropriately
        analysis = target(fun(data),
                          transform = cross(fun = !!rlang::syms(methods$target),
                                            data = !!rlang::syms(datasets$target))
        ),
        # create a list of the created `analysis` objects, grouping by the `fun`
        # that made them - this keeps the results from the different methods
        # separated, so that the reports/syntheses can handle the right outputs
        results = target(MATSS::collect_analyses(list(analysis)),
                         transform = combine(analysis, .by = fun)),
        ...
    )
}

#' @title Generate a Drake Plan for Datasets
#' 
#' @param data_path where to get the downloaded retriever datasets
#' @param include_retriever_data whether to include retriever-downloaded data
#' @param include_bbs_data whether to include BBS data
#' @param include_gpdd_data whether to include gpdd data
#' @param include_biotime_data whether to include biotime data
#' @inheritParams build_bbs_datasets_plan
#' 
#' @return a drake plan (i.e. a tibble) specifying the targets and commands 
#'   for gathering datasets
#' 
#' @export
#' 
build_datasets_plan <- function(data_path = get_default_data_path(), 
                                include_retriever_data = FALSE,
                                include_bbs_data = FALSE,
                                bbs_subset = NULL,
                                include_gpdd_data = FALSE, 
                                include_biotime_data = FALSE)
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
        datasets <- datasets %>%
            dplyr::bind_rows(
                drake::drake_plan(
                    portal_data = get_portal_rodents(),
                    sdl_data = get_sdl_data(path = !!data_path),
                    mtquad_data = get_mtquad_data(path = !!data_path)
                )
            )
    }
    
    if (include_bbs_data) {
        bbs_datasets = build_bbs_datasets_plan(data_path = data_path, 
                                               bbs_subset = bbs_subset)
        
        datasets <- datasets %>%
            dplyr::bind_rows(bbs_datasets)
    }
    
    if (include_gpdd_data) {
        gpdd_datasets = build_gpdd_datasets_plan()
        
        datasets <- datasets %>%
            dplyr::bind_rows(gpdd_datasets)
    }
    if (include_biotime_data) {
        biotime_datasets = build_biotime_datasets_plan(data_path = data_path)
        
        datasets <- datasets %>%
            dplyr::bind_rows(biotime_datasets)
    }
    
    return(datasets)
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
build_bbs_datasets_plan <- function(data_path = get_default_data_path(), bbs_subset = NULL)
{
    routes_and_regions_file <- file.path(data_path, "breed-bird-survey-prepped", "routes_and_regions_table.csv")
    
    if (!file.exists(routes_and_regions_file)) {
        message("preprocessing bbs timeseries data")
        prepare_bbs_ts_data(path = data_path, bbs_subset = bbs_subset)
    }
    
    routes_and_regions <- utils::read.csv(routes_and_regions_file, colClasses = "character")
    
    if (!is.null(bbs_subset)) {
        routes_and_regions <- routes_and_regions[bbs_subset, ]
    }
    
    bbs_datasets <- drake::drake_plan(
        bbs_data_rtrg = target(get_bbs_route_region_data(route, region, path = !!data_path),
                               transform = map(route = !!rlang::syms(routes_and_regions$route),
                                               region = !!rlang::syms(routes_and_regions$bcr)
                               )
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
        gpdd_data_rtrg = target(get_gpdd_data(location_id = location_id, timeperiod_id = timeperiod_id),
                                transform = map(location_id = !!rlang::syms(locations$LocationID),
                                                timeperiod_id = !!rlang::syms(locations$TimePeriodID)
                                )
        )
    )
    return(gpdd_datasets)
}

#' @title Generate a Drake Plan for Biotime Datasets
#' 
#' @inheritParams build_datasets_plan
#' @inheritParams get_biotime_data
#' 
#' @return a drake plan (i.e. a tibble) specifying the targets and commands 
#'   for gathering Biotime datasets
#' 
#' @export
#' 
build_biotime_datasets_plan <- function(data_path = get_default_data_path())
{
    dataset_file <- file.path(data_path, "biotimesql", "biotimesql_citation1.csv")
    dataset_list <- utils::read.csv(dataset_file, colClasses = "character")
    
    biotime_datasets <- drake::drake_plan(
        biotime_data_rtrg = target(get_biotime_data(dataset = dataset, path = !!data_path),
                                   transform = map(dataset = !!rlang::syms(unique(dataset_list$study_id)))
        )
    )
    return(biotime_datasets)
}
