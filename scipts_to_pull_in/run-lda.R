# this is the part that would be automated?

source('R/lda-function.R')

run_LDA(abundance_data_path = "data/portal/portal_controls_data.csv",
            max_topics = 6, ncores = 4, nseeds = 20)

load('output/portal_controls_data_LDA.Rds')
