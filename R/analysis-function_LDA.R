run_LDA <- function(data, 
                    max_topics = 6, nseeds = 200, 
                    ncores = 4)
{
    topics_vector <- c(2:max_topics)
    
    #### Run LDAs ####
    LDA_models = LDATS::parLDA(data = data, ntopics = topics_vector,
                               nseeds = nseeds, ncores = ncores)
    
    #### Select the best LDA (AICc) ####
    selected = LDATS:::LDA_select(lda_models = LDA_models, LDA_eval = quote(AIC), 
                                  correction = TRUE, LDA_selector = quote(min))
    
    return(selected)
}
