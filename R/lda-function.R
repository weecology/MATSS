library(LDATS)

run_LDA <- function(abundance_data_path, 
                    max_topics = 6, nseeds = 200, 
                    ncores = 4){
    
    #### Load data ####
    abundance_data <- read.csv(abundance_data_path, 
                               stringsAsFactors = F)
    
    topics_vector <- c(2:max_topics)
    
    #### Run LDAs ####
    LDA_models = LDATS::parLDA(data = dplyr::select(abundance_data, -timestep, -date), ntopics =  topics_vector,
                               nseeds = nseeds, ncores = ncores)
    
    #### Select the best LDA (AICc) ####
    selected = LDATS:::LDA_select(lda_models = LDA_models, LDA_eval = quote(AIC), correction = TRUE,
                                  LDA_selector = quote(min))
    
    #### Save the LDA ####
    
    data_name <- strsplit(abundance_data_path, '/')
    data_name <- unlist(data_name)[3]
    data_name <- strsplit(data_name, '.csv')
    data_name <- unlist(data_name)[1]
    
    save_path <- paste0('output/', data_name, '_LDA.Rds')
    
    save(selected, file = save_path)
}
