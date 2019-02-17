library(MATSS)
library(dplyr)
library(drake)

## Make sure we have downloaded the raw datasets from retriever first
if (FALSE)
{
    install_retriever_data("breed-bird-survey")
    install_retriever_data("veg-plots-sdl")
    install_retriever_data("mapped-plant-quads-mt")
}

## Clean and transform the data into the appropriate format
datasets <- bind_rows(plan_datasets(), 
                      drake_plan(bad_portal = portal_data[[1]])
)

## Analysis methods
methods <- drake_plan(
    dataset_summary = function(dataset) {ts_summary_drake(dataset)},
    lda = function(dataset) {run_LDA(dataset, max_topics = 6, nseeds = 20)}
)

## Define how results are collected
collect <- function(list_of_results, plan)
{
    names(list_of_results) <- all.vars(match.call()$list_of_results)
    list_of_results
}

## The combination of each method x dataset
analyses <- drake_plan(
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
    results = target(collect(list(analysis), ignore(analyses)),
                     transform = combine(analysis, .by = fun)),
    trace = TRUE
)

## Summary reports
# I don't quite understand the pathing here... - Hao
reports <- drake_plan(
    lda_report = rmarkdown::render(
        knitr_in("analysis/lda_report.Rmd")
    )
)

## The entire pipeline
pipeline <- bind_rows(datasets, methods, analyses, reports)

## Set up the cache and config
db <- DBI::dbConnect(RSQLite::SQLite(), here::here("output", "drake-cache.sqlite"))
cache <- storr::storr_dbi("datatable", "keystable", db)

## View the graph of the plan
if (interactive())
{
    config <- drake_config(pipeline, cache = cache)
    sankey_drake_graph(config, build_times = "none")  # requires "networkD3" package
    vis_drake_graph(config, build_times = "none")     # requires "visNetwork" package
}

## Run the pipeline
make(pipeline, cache = cache)

## Run the pipeline (parallelized)
# future::plan(future::multiprocess)
# make(pipeline, 
#      force = TRUE, 
#      cache = cache,
#      verbose = 2,
#      parallelism = "future",
#      jobs = 2,
#      caching = "master") # Important for DBI caches!
