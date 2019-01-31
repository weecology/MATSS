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
    mtquad_data_tables = rdataretriever::fetch("mapped-plant-quads-mt"), 
    strings_in_dots = "literals"
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
    lda = run_LDA(dataset__, max_topics = 6, nseeds = 20)
)

## The combination of each method x dataset 
analyses <- plan_analyses(methods, data = datasets)

## Combine LDA analyses together
lda_results_plan <- gather_plan(
    plan = analyses, 
    target = "lda_results", 
    gather = "list"
)

## Summary reports
reports <- drake_plan(
    lda_report = rmarkdown::render(knitr_in("lda_report.Rmd"), 
                                   output_file = file_out("lda_report.md"))
)

## The entire pipeline
pipeline <- rbind(datasets_raw, datasets, analyses, lda_results_plan, reports)

## View the graph of the plan
if (interactive())
{
    config <- drake_config(pipeline)
    vis_drake_graph(config)
}

## Run the pipeline
future::plan(future::multiprocess)
db <- DBI::dbConnect(RSQLite::SQLite(), "drake-cache.sqlite")
cache <- storr::storr_dbi("datatable", "keystable", db)
make(pipeline,
     cache = cache,
     verbose = 2,
     parallelism = "future",
     jobs = 2,
     caching = "master" # Important for DBI caches!
)
