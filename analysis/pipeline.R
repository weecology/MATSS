library(MATSS)
library(dplyr)
library(drake)

## Read in the maizuru community data from a csv file
get_maizuru_data <- function()
{
    raw_data <- read.csv(here::here("data", "Maizuru_dominant_sp.csv"))
    
    list(abundance = dplyr::select(raw_data, -date_tag, -surf.t, -bot.t, -Y, -M, -D) %>%
             mutate_all(~round(. + 1e-10)), 
         covariates = dplyr::select(raw_data, date_tag, surf.t, bot.t, Y, M, D))
}

## Get raw data
datasets_raw <- drake_plan(
    bbs_data_tables = rdataretriever::fetch("breed-bird-survey"),
    sdl_data_tables = rdataretriever::fetch("veg-plots-sdl"),
    mtquad_data_tables = rdataretriever::fetch("mapped-plant-quads-mt")
)

## Clean and transform the data into the appropriate format
datasets <- drake_plan(
    portal_data = get_portal_rodents(),
    maizuru_data = get_maizuru_data(), 
    jornada_data = process_jornada_data(),
    sgs_data = process_sgs_data(),
    bbs_data = get_bbs_data(bbs_data_tables, region = 7),
    sdl_data = get_sdl_data(sdl_data_tables),
    mtquad_data = get_mtquad_data(mtquad_data_tables)
)

## Analysis methods
methods <- drake_plan(
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
    results = target(collect(analysis, ignore(analyses)), 
                     transform = combine(analysis, .by = fun)), 
    trace = TRUE
)

## Summary reports
reports <- drake_plan(
    lda_report = rmarkdown::render(knitr_in("lda_report.Rmd"), 
                                   output_file = file_out("lda_report.md"))
)

## The entire pipeline
pipeline <- bind_rows(datasets_raw, datasets, methods, analyses, reports)

## View the graph of the plan
if (interactive())
{
    config <- drake_config(pipeline)
    sankey_drake_graph(config)           # requires "networkD3" package
    vis_drake_graph(config)              # requires "visNetwork" package
}

## Run the pipeline
db <- DBI::dbConnect(RSQLite::SQLite(), here::here("output", "drake-cache.sqlite"))
cache <- storr::storr_dbi("datatable", "keystable", db)

future::plan(future::multiprocess)
make(pipeline,
     force = TRUE, 
     cache = cache,
     verbose = 2,
     parallelism = "future",
     jobs = 2,
     caching = "master" # Important for DBI caches!
)
