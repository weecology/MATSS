Summary report
================
Renata Diaz, Juniper Simonis
2/16/2019

## Read in the results

``` r
# define where the cache is located
db <- DBI::dbConnect(RSQLite::SQLite(), here::here("output", "drake-cache.sqlite"))
cache <- storr::storr_dbi("datatable", "keystable", db)

results_dataset_summary <- readd(results_dataset_summary, cache = cache)
```

## Errors

Find datasets that threw errors and list them:

    ## [1] "analysis_dataset_summary_bad_portal"
    ## [1] "Incorrect data structure"

From here, choose aspects of results\_dataset\_summary to present in
report.
