library(MATSS)
library(dplyr)
library(drake)

lda_analysis <- drake::drake_plan(
    portal_data_raw = get_portal_rodents(), 
    portal_data = portal_data_raw %>%
        select(-period, -censusdate), 
    portal_lda = run_LDA(portal_data, 
                         max_topics = 6, ncores = 4, nseeds = 20),
    lda_report = rmarkdown::render(input = 'lda_report.Rmd', output_file = 'lda_report.html')
)

source('R/analysis-function_LDA.R')
source('R/data-function_portal.R')

drake::make(lda_analysis)
