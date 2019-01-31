library(MATSS)
library(dplyr)
library(drake)

## Get raw data
datasets_raw <- drake_plan(
    portal_data_raw = get_portal_rodents(), 
    maizuru_data_raw = get_maizuru_data_raw(),
    jornada_data_raw = process_jornada_data(),
    sgs_data_raw = process_sgs_data()
)

## Clean and transform the data into the appropriate format
datasets <- drake_plan(
    portal_data = portal_data_raw,
    maizuru_data = maizuru_data_raw,
    jornada_data = jornada_data_raw,
    sgs_data = sgs_data_raw
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
    lda_report = rmarkdown::render(input = "lda_report.Rmd", 
                                   output_file = "lda_report.md")
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

