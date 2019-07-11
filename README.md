
<!-- README.md is generated from README.Rmd. Please edit that file -->

# MATSS

[![Build
Status](https://travis-ci.org/weecology/MATSS.svg?branch=master)](https://travis-ci.org/weecology/MATSS)
[![Coverage
status](https://codecov.io/gh/weecology/MATSS/branch/master/graph/badge.svg)](https://codecov.io/github/weecology/MATSS?branch=master)

## Overview

The **`MATSS`** package is intended to support Macroecological Analysis
of Time Series Structure. We provide functions to:

  - gather ecological time series datasets
  - perform basic processing and summaries of those datasets
  - build an analytical pipeline to conduct macroecological analyses on
    those datasets
  - create template reports for collating results and produce syntheses

## Contributing

For more information about contributing code, datasets, or analyses,
please check out the [Contributing Guide](CONTRIBUTING.md).

## Installation

You can install **`MATSS`** from github with:

``` r
# install.packages("remotes")
remotes::install_github("weecology/MATSS", build_opts = c("--no-resave-data", "--no-manual"))
```

This package relies on the development version of the `rdataretriever`
package to install datasets. Installation of this package takes a few
extra steps because it runs a Python package behind the scenes. Follow
the installation instructions on the [`rdataretriever`
README](https://github.com/ropensci/rdataretriever).

## Datasets

**`MATSS`** pulls data from a variety of sources, including:

  - 10 individual datasets that we’ve added,
  - the North American Breeding Bird Survey database (spanning 2587
    separate datasets),
  - the Global Population Dynamics Database (spanning 120 separate
    datasets),
  - and the BioTime database (spanning 361 separate datasets).

Combined, there are **84052** individual time series across all of these
data sources.

## Getting Started

To get started with the data or analysis templates, we recommend you
take a look at our [Getting Started
vignette](https://weecology.github.io/MATSS/articles/MATSS.html) for
more details about how to interface with the datasets, use Drake to
create workflows, and create research compendia.

If you have the **`MATSS`** package installed, you can also view the
vignette from within R:

``` r
vignette("MATSS")
```

## Example Use Cases

Here are some examples of using **`MATSS`** to create research
compendia:

  - [MATSS-LDATS](https://github.com/weecology/MATSS-LDATS) applies the
    [**`LDATS`**](https://github.com/weecology/LDATS) package to
    investigate changepoints in community dynamics across the datasets
    in **`MATSS`**
  - [MATSS-Forecasting](https://github.com/weecology/MATSS-forecasting)
    investigates which properties are associated with the predictability
    of population time series across the datasets in **`MATSS`**
