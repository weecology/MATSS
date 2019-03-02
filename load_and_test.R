# Used for continous testing platform
# Install analysis packages using pacman
# Pacman will load the packages and install
# the packaes not available

library(devtools)
# Install pacman if it isn't already installed
if ("pacman" %in% rownames(installed.packages()) == FALSE) install.packages("pacman")
# suppressMessages(install.packages("RMariaDB",  quiet = TRUE))
suppressMessages(
	pacman::p_load(devtools, RCurl, readr, rmarkdown,
	testthat, tidyverse, RSQLite,
	# DBI, RPostgreSQL,
	magrittr, ggplot2, lubridate,
 	dplyr, tidyr, RSQLite, forecast,
 	networkD3, here, tibble, rlang)
)

# Install GitHub packages
 
devtools::install_github("weecology/portalr")
devtools::install_github("ropensci/drake")
devtools::install_github("rstudio/reticulate")
devtools::install_github("ropensci/rdataretriever")
devtools::install_github("r-lib/usethis")
devtools::install_github("weecology/LDATS")
library(devtools)
library(portalr)
library(drake)
library(reticulate)
library(rdataretriever)
library(usethis)
library(LDATS)
install.packages(".", repos = NULL, type="source")
# Test package
# test_dir("tests/testthat", reporter = c("check", "progress"))

# Try old way
# This will not work well for reporting and fail/pass 
devtools::test()

