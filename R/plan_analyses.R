#' @title Collect Analyses Together into a Tibble
#' 
#' @description This is a helper function to accompany [`plan_analyses`]: it is 
#'   necessary to collect all of the results that are produced by the drake 
#'   plan.
#'   
#'   This function strives to be intelligent about the format of the individual 
#'   results. For output from [`analysis_wrapper`] that already has information 
#'   about the method and dataset, we can just combine them. Otherwise, we 
#'   parse the name of the object for the method and the dataset, to format 
#'   into a structure similar to the output from [`analysis_wrapper`].
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
    if (!is_structured_results_list(list_of_results))
    {
        results_names <- all.vars(match.call()$list_of_results)
        
        
        
        
        list_of_results
        
    }
    dplyr::bind_rows(list_of_results)
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
        analysis = drake::target(invoke(fun, data),
                                 transform = cross(fun = !!rlang::syms(methods$target),
                                                   data = !!rlang::syms(datasets$target))
        ),
        # create a list of the created `analysis` objects, grouping by the `fun`
        # that made them - this keeps the results from the different methods
        # separated, so that the reports/syntheses can handle the right outputs
        results = drake::target(dplyr::bind_rows(analysis),
                                transform = combine(analysis, .by = fun)),
        ...
    )
}
