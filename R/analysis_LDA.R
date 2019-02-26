#' @name run_LDA
#' @title Run Latent Dirichlet Allocation on Tabular Data
#' @description Test the Latent Dirichlet Allocation (LDA) model on the data 
#'   with different number of topics (from 2 to `max_topics`), select the best 
#'   one using AIC, and return the model object that is selected.
#' 
#' @param data a data.frame or tibble; each row is an observation (e.g. in 
#'   time or space), and each column is a variable. Here, the common usage is  
#'   for each column to be a species or taxon, and each row to be an observed 
#'   sample. In the original specification for LDA, each row is a document, 
#'   and each column is a word, with the entries being the counts of the words
#'   in each document. 
#' @param max_topics the maximum number of topics to try (the function will 
#'   test a number of topics from 2 to `max_topics`)
#' @inheritParams LDATS::LDA_set
#' 
#' @return the best fit model object, from running `LDATS::parLDA()`
#' @export
#' 
run_LDA <- function(data, 
                    max_topics = 6, nseeds = 200,
                    control = LDATS::LDA_controls_list())
{
    if (!check_data_format(data))
    {
        wrongFormat = simpleWarning("Incorrect data structure, see data-formats vignette")
        tryCatch(warning(wrongFormat), finally = return('Incorrect data structure'))
    } 
    
    #### Run LDAs ####
    abundances <- data$abundance
    topics_vector <- seq(from = 2, to = max_topics, by = 1)
    LDA_models = LDATS::LDA_set(document_term_table = abundances, 
                                topics = topics_vector,
                                nseeds = nseeds, control = control)
    
    #### Select the best LDA (AIC) ####
    LDATS::select_LDA(LDA_models = LDA_models, control = control)
}
