
<!-- README.md is generated from README.Rmd. Please edit that file -->

# MATSS 0.1.1

<!-- badges: start -->

[![Build
Status](https://travis-ci.org/weecology/MATSS.svg?branch=master)](https://travis-ci.org/weecology/MATSS)
[![License](https://img.shields.io/badge/license-MIT-blue.svg)](https://raw.githubusercontent.com/weecology/MATSS/master/LICENSE)
[![Coverage
status](https://codecov.io/gh/weecology/MATSS/branch/master/graph/badge.svg)](https://codecov.io/github/weecology/MATSS?branch=master)
[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.3333008.svg)](https://doi.org/10.5281/zenodo.3333008)
<!-- badges: end -->

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

## Acknowledgments

We thank Erica Christensen and Joan Meiners for their contributions and
input on early prototypes of this project. This project would not be
possible without the support of Henry Senyondo and the
[retriever](https://www.data-retriever.org/) team.

Package development is supported through various funding sources:
including the [Gordon and Betty Moore Foundation’s Data-Driven Discovery
Initiative](http://www.moore.org/programs/science/data-driven-discovery),
[Grant GBMF4563](http://www.moore.org/grants/list/GBMF4563) to E. P.
White (supporting also the time of J. Simonis and H. Ye), the [National
Science Foundation](http://nsf.gov/), [Grant
DEB-1622425](https://www.nsf.gov/awardsearch/showAward?AWD_ID=1622425)
to S. K. M. Ernest, and a [National Science Foundation Graduate Research
Fellowship](https://www.nsfgrfp.org/) (No.
[DGE-1315138](https://www.nsf.gov/awardsearch/showAward?AWD_ID=1315138)
and
[DGE-1842473](https://www.nsf.gov/awardsearch/showAward?AWD_ID=1842473))
to R. Diaz.
