library(MATSS)
library(drake)
library({{{package}}})

## set working directory to project folder
setwd(here::here())

## include the functions in packages as dependencies
#  - this is to help Drake recognize that targets need to be rebuilt if the 
#    functions have changed
expose_imports(MATSS)
expose_imports({{{package}}})

## download the datasets
#  - toggle the following variable to use downloaded datasets
use_downloaded_datasets <- FALSE
if (use_downloaded_datasets)
{
    download_datasets()
}

## a Drake plan for creating the datasets
#  - these are the default options, which don't include downloaded datasets
datasets <- build_datasets_plan(include_retriever_data = use_downloaded_datasets, 
                                include_bbs_data = use_downloaded_datasets, 
                                include_gpdd = use_downloaded_datasets, 
                                include_biotime_data = use_downloaded_datasets)

## a Drake plan that defines the methods
methods <- drake_plan(
    simpson_index = {{{package}}}::compute_simpson_index,
    linear_trend = {{{package}}}::compute_linear_trend
)

## a Drake plan for the analyses (each combination of method x dataset)
analyses <- build_analyses_plan(methods, datasets)

## a Drake plan to collect citation info from datasets
references <- build_references_plan(datasets)

## a Drake plan for the Rmarkdown report
#  - we use `knitr_in()` 
reports <- drake_plan(
    report = rmarkdown::render(
        knitr_in("analysis/report.Rmd"), 
        output_file("analysis/report.md")
    )
)

## The full workflow
workflow <- bind_plans(
    datasets,
    methods,
    analyses,
    references, 
    reports
)

## Visualize how the targets depend on one another
if (interactive())
{
    if (require(networkD3, quietly = TRUE))
        sankey_drake_graph(workflow, build_times = "none", targets_only = TRUE)
    if (require(visNetwork, quietly = TRUE))
        vis_drake_graph(workflow, build_times = "none", targets_only = TRUE)
}

## Run the workflow
make(workflow)
