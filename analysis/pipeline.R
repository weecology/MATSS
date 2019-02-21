library(MATSS)
library(dplyr)
library(drake)

## Make sure we have downloaded the raw datasets from retriever first
if (check_default_data_path())
{
    message("Using ", usethis::ui_code(get_default_data_path()), " as the downloaded data directory.")
    suppressMessages({
        install_retriever_data("breed-bird-survey")
        install_retriever_data("veg-plots-sdl")
        install_retriever_data("mapped-plant-quads-mt")
    })
}

## Clean and transform the data into the appropriate format
datasets <- bind_rows(build_datasets_plan(include_downloaded_data = TRUE), 
                      drake_plan(bad_portal = portal_data[[1]])
)

## Analysis methods
methods <- drake_plan(
    lda = function(dataset) {run_LDA(dataset, max_topics = 6, nseeds = 20)}
)

analyses <- build_analyses_plan(methods, datasets)

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
