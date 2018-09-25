library(MATSS)
library(dplyr)
library(drake)

lda_analysis <- drake::drake_plan(
    portal_data_raw = get_portal_rodents(), 
    portal_data = portal_data_raw %>%
        select(-period, -censusdate), 
    portal_lda = run_LDA(portal_data, 
                         max_topics = 6, ncores = 4, nseeds = 20)
)

drake::make(lda_analysis)