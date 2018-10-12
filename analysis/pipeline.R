library(MATSS)
library(dplyr)
library(drake)

## Read in the maizuru community data from a csv file
get_maizuru_data <- function()
{
    read.csv(here::here("data", "Maizuru_dominant_sp.csv")) %>%
        select(-date_tag, -surf.t, -bot.t, -Y, -M, -D) %>%
        mutate_all(~round(. + 1e-10))
}

## Clean and transform the data into the appropriate format
datasets <- drake_plan(
    portal_data = get_portal_rodents() %>%
        select(-period, -censusdate),
    maizuru_data = get_maizuru_data()
)

## Analysis methods
methods <- drake_plan(
    lda = run_LDA(dataset__, max_topics = 6, ncores = 4, nseeds = 20)
)

## The combination of each method x dataset 
analyses <- plan_analyses(methods, data = datasets)

## The entire pipeline
pipeline <- rbind(datasets, analyses)

## View the graph of the plan
if (interactive())
{
    config <- drake_config(pipeline)
    vis_drake_graph(config)
}

make(pipeline)

