library(tidyverse)

#### load scripts ----
data_scripts <- list.files(here::here("data_scripts"))
source(here::here("data_scripts", data_scripts))
# now we have a function defined as get_data() that returns a tibble with portal 
# abundances, eventually we will want to automatically keep track of these 
# functions so that they can be called and the output saved

analysis_scripts <- list.files(here::here("analysis_scripts"))
source(here::here("analysis_scripts", analysis_scripts))

report_scripts <- list.files(here::here("report_scripts"))
source(here::here("report_scripts", report_scripts))

#### get data objects ----
data_portal <- get_data()

#### run LDATS script ----
LDA_portal <- run_LDA_on_data(data_portal)

#### generate a report ----
make_LDA_report()
