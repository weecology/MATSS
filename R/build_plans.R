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
#' 
#' @return a drake plan (i.e. a tibble) specifying the targets and commands 
#'   for all the analyses and the collected results (grouping the outputs from 
#'   each method into a single list)
#' 
#' @export
#' 
build_analyses_plan <- function(methods, datasets)
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
        trace = TRUE
    )
}

#' @title Generate a Drake Plan for Datasets
#' 
#' @return a drake plan (i.e. a tibble) specifying the targets and commands 
#'   for gathering datasets
#' 
#' @export
#' 
build_datasets_plan <- function()
{
    drake::drake_plan(
        portal_data = get_portal_rodents(),
        maizuru_data = get_maizuru_data(),
        jornada_data = get_jornada_data(),
        sgs_data = get_sgs_data(),
        bbs_data = get_bbs_data(region = 7, folder_path = 'data'),
        sdl_data = get_sdl_data(folder_path = 'data'),
        mtquad_data = get_mtquad_data(folder_path = 'data')
    )
}