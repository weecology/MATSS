library(MATSS)
library(dplyr)
library(drake)

## Read in the maizuru community data from a csv file
get_maizuru_data_raw <- function()
{
    read.csv(here::here("data", "Maizuru_dominant_sp.csv"))
}

## Get raw data
datasets_raw <- drake_plan(
    portal_data_raw = get_portal_rodents(), 
    maizuru_data_raw = get_maizuru_data_raw(),
    jornada_data_raw = process_jornada_data()
)

## Clean and transform the data into the appropriate format
datasets <- drake_plan(
    portal_data = portal_data_raw %>%
        select(-period, -censusdate),
    maizuru_data = maizuru_data_raw %>% 
        select(-date_tag, -surf.t, -bot.t, -Y, -M, -D) %>%
        mutate_all(~round(. + 1e-10)),
    jornada_data = jornada_raw$abundance
)

## Analysis methods
methods <- drake_plan(
    lda = run_LDA(dataset__, max_topics = 6, ncores = 2, nseeds = 20)
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
    lda_report = rmarkdown::render(input = 'lda_report.Rmd', 
                                   output_file = 'lda_report.md')
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
make(pipeline, verbose = 2)

