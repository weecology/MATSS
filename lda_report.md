LDA report
================
Renata Diaz
10/12/2018

``` r
library(MATSS)
```

    ## Please look at our data formats by running `vignette("data-formats")`

``` r
library(drake)
```

Plot LDAS
---------

    ## [1] "lda_portal_data"

![](lda_report_files/figure-markdown_github/plot%20LDA-1.png)

    ## [1] "lda_maizuru_data"

![](lda_report_files/figure-markdown_github/plot%20LDA-2.png)

    ## [1] "lda_jornada_data"

![](lda_report_files/figure-markdown_github/plot%20LDA-3.png)

Summarize LDA results
---------------------

``` r
lda_summary <- as.data.frame(names(lda_results))
lda_summary$ntopics <- NA
lda_summary$ntimeseries <- NA
lda_summary$ntimesteps <- NA

for(i in seq(lda_results)) {
    lda_summary$ntopics[i] <- lda_results[[i]][1]$k@k
    lda_summary$ntimeseries[i] <- as.integer(length(lda_results[[i]][1]$k@terms))
    lda_summary$ntimesteps[i] <- lda_results[[i]][1]$k@wordassignments$nrow
}

lda_summary
```

    ##   names(lda_results) ntopics ntimeseries ntimesteps
    ## 1    lda_portal_data       5          21        295
    ## 2   lda_maizuru_data       6          15        285
    ## 3   lda_jornada_data       6          17         24

![](lda_report_files/figure-markdown_github/plot%20lda%20summary-1.png)![](lda_report_files/figure-markdown_github/plot%20lda%20summary-2.png)
