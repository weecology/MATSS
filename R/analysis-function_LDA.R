#' @name run_LDA
#' @title Run Latent Dirichlet Allocation on Tabular Data
#' @description Test the Latent Dirichlet Allocation (LDA) model on the data 
#'   with different number of topics (from 2 to `max_topics`), select the best 
#'   one using AIC, and return the model object that is selected.
#' 
#' @param data a data.frame or tibble; each row is an observation (e.g. in time 
#'   or space), and each column is a variable. Here, the common usage is for 
#'   each column to be a species or taxon, and each row to be an observed 
#'   sample. In the original specification for LDA, each row is a document, and 
#'   each column is a word, with the entries being the counts of the words in 
#'   each document. 
#' @param max_topics the maximum number of topics to try (the function will 
#'   test a number of topics from 2 to `max_topics`)
#' @inheritParams LDATS::parLDA
#' 
#' @return the best fit model object, from running `LDATS::parLDA()`
#' 
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
