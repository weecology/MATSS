context("Check LDA analysis function")

# create test data
n <- 100 # number of samples
topics <- matrix(c(0.4, 0.4, 0.2, 0, 0, 0, 
                   0, 0, 0, 0, 0.5, 0.5), 
                 nrow = 2, byrow = TRUE)
m <- NROW(topics) # number of topics
topic_prop <- matrix(abs(rnorm(n * m)), nrow = n)
topic_prop <- topic_prop / 
    matrix(rep(rowSums(topic_prop), m), nrow = n)

total_abundance <- rpois(n, 200)
abundance <- floor(total_abundance * topic_prop %*% topics)


test_that("run_LDA function works", {
    # does run_LDA check data format?
    expect_warning(output <- run_LDA(abundance, nseeds = 20), 
                   "Incorrect data structure, see data-formats vignette")
    expect_equal(output, "Incorrect data structure")
    
    # does run_LDA run and produce a valid output
    data <- list(abundance = data.frame(abundance))
    expect_error(output <- run_LDA(data, max_topics = 2, nseeds = 50), NA)
    expect_true(all(c("LDA_set", "list") %in% class(output)))
    lda_model <- output[[1]]
    
    # does the fitted model have the right # of topics
    expect_equal(lda_model@k, m)
    
    # see if we need to swap order of topics
    model_topics <- exp(lda_model@beta)
    idx <- seq_len(m)
    if (sum(sum(abs(model_topics - topics))) > 
        sum(sum(abs(model_topics[rev(idx), ] - topics))))
    {
        idx <- rev(idx)
    }
    expect_equal(model_topics[idx, ], topics, tolerance = 0.05)
    expect_equal(lda_model@gamma[, idx], topic_prop, tolerance = 0.1)
})
