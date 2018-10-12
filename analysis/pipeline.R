library(MATSS)
library(dplyr)
library(drake)

lda_analysis <- drake::drake_plan(
    portal_data_raw = get_portal_rodents(), 
    portal_data = portal_data_raw %>%
        select(-period, -censusdate), 
    portal_lda = run_LDA(portal_data, 
                         max_topics = 6, ncores = 4, nseeds = 20),
    lda_report = knitr::knit(knitr_in('lda_report.Rmd'), file_out('lda_report.html'), quiet = T)
)

source('R/analysis-function_LDA.R')
source('R/data-function_portal.R')

drake::make(lda_analysis)
