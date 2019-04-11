# MATSS

[![Build Status](https://travis-ci.org/weecology/MATSS.svg?branch=master)](https://travis-ci.org/weecology/MATSS)
[![Coverage
status](https://codecov.io/gh/weecology/MATSS/branch/master/graph/badge.svg)](https://codecov.io/github/weecology/MATSS?branch=master)

## Overview
The *MATSS* package is intended to support Macroecological Analysis of Time Series Structure. We provide functions to :
* gather ecological time series datasets
* perform basic processing and summaries of those datasets
* build an analytical pipeline to conduct macroecological analyses on those datasets
* create template reports for collating results and produce syntheses

## Contributing

For more information about contributing code, datasets, or analyses, please check out the [Contributing Guide](CONTRIBUTING.md).

## Installation

You can install the `MATSS` package from github with:

``` r
# install.packages("devtools")
devtools::install_github("weecology/MATSS")
```

This package relies on the development version of the `rdataretriever` package
to install datasets. Installation of this package takes a few extra steps
because it runs a Python package behind the scenes. Follow the installation
instructions on the [`rdataretriever` README](https://github.com/ropensci/rdataretriever).


## Examples

{add examples of using the `MATSS` package to get data and/or use the analysis methods}

## Running the Analyses
Note: This entire section is deprecated and will be moved over into https://github.com/weecology/MATSS-LDATS


To run the analysis component of this project, you will want to clone or copy the repository, *in addition* to installing the package. (*note that you can also install the package locally from the files*)

The file `analysis/pipeline.R` has a full script that generates the Drake plan for the analyses and then runs it. When the code is run, these steps occur in sequence:
(1) R creates a list of objects that need to be created and their dependencies (whether certain outputs depend on other outputs)
(2) R checks the list of objects and code against its database of objects (usually stored in the `.drake` hidden folder). If any objects are out of date (because they are missing or the code / dependencies have changed), then they are queued up to be re-created.
(3) R makes any objects that it needs to, including final reports, such as [LDA report](lda_report.md).
