LDA report
================
Renata Diaz
10/12/2018

## Read in the results

``` r
# define where the cache is located
db <- DBI::dbConnect(RSQLite::SQLite(), here::here("output", "drake-cache.sqlite"))
cache <- storr::storr_dbi("datatable", "keystable", db)

lda_results <- readd(results_lda, cache = cache)
```

## Errors

Find LDAs that threw errors and remove them:

    ## [1] "analysis_lda_bad_portal"
    ## [1] "Incorrect data structure"

## Plot LDAS

    ## [1] "analysis_lda_portal_data"

![](lda_report_files/figure-gfm/plot%20LDA-1.png)<!-- -->

    ## [1] "analysis_lda_maizuru_data"

![](lda_report_files/figure-gfm/plot%20LDA-2.png)<!-- -->

    ## [1] "analysis_lda_jornada_data"

![](lda_report_files/figure-gfm/plot%20LDA-3.png)<!-- -->

    ## [1] "analysis_lda_sgs_data"

![](lda_report_files/figure-gfm/plot%20LDA-4.png)<!-- -->

    ## [1] "analysis_lda_bbs_data"

![](lda_report_files/figure-gfm/plot%20LDA-5.png)<!-- -->

    ## [1] "analysis_lda_sdl_data"

![](lda_report_files/figure-gfm/plot%20LDA-6.png)<!-- -->

    ## [1] "analysis_lda_mtquad_data"

![](lda_report_files/figure-gfm/plot%20LDA-7.png)<!-- -->

## Summarize LDA results

``` r
lda_summary <- as.data.frame(names(lda_results))
lda_summary$ntopics <- NA
lda_summary$ntimeseries <- NA
lda_summary$ntimesteps <- NA

for (i in seq(lda_results))
{
    lda_summary$ntopics[i] <- lda_results[[i]][1]$k@k
    lda_summary$ntimeseries[i] <- as.integer(length(lda_results[[i]][1]$k@terms))
    lda_summary$ntimesteps[i] <- lda_results[[i]][1]$k@wordassignments$nrow
}

lda_summary
```

    ##          names(lda_results) ntopics ntimeseries ntimesteps
    ## 1  analysis_lda_portal_data       5          21        295
    ## 2 analysis_lda_maizuru_data       6          15        285
    ## 3 analysis_lda_jornada_data       6          17         24
    ## 4     analysis_lda_sgs_data       4          11         13
    ## 5     analysis_lda_bbs_data       6         164      69564
    ## 6     analysis_lda_sdl_data       6          98         22
    ## 7  analysis_lda_mtquad_data       6          42         14

![](lda_report_files/figure-gfm/plot%20lda%20summary-1.png)<!-- -->![](lda_report_files/figure-gfm/plot%20lda%20summary-2.png)<!-- -->
