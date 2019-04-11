context("Tests of Wrapper Functions")

test_that("analysis_wrapper works for simple functions", {
    # setup sample inputs
    data <- get_sgs_data()
    fun <- function(ts) {
        tibble::tibble(n = NROW(ts), 
                       mean = mean(ts), 
                       sd = sd(ts))
    }
    
    # check if it worked properly and had the right size
    expect_error(f <- analysis_wrapper(fun), NA)
    expect_error(output <- f(data), NA)
    expect_equal(dim(output), c(1, 5))
    
    # check results data.frame
    expect_error(results <- output$results[[1]], NA)
    expect_equal(dim(results), c(11, 4))
    expect_identical(results$id, names(data$abundance))
    expect_identical(output$dataset, "data")
    expect_identical(output$method, "fun")
    expect_identical(output$args[[1]], list())
    
    # check metadata
    expect_identical(output$metadata[[1]], data$metadata)
    
    # check digest
    expect_known_hash(digest::digest(output), "a5b8d5c75c")
})

test_that("analysis_wrapper preserves arguments correctly", {
    # setup sample inputs
    data <- get_sgs_data()
    CI_levels <- c(0.05, 0.95)
    
    # create our different methods
    expect_error(quantiles_default <- analysis_wrapper(quantile), NA)
    expect_error(quantiles_named <- analysis_wrapper(quantile, probs = c(0.05, 0.95)), NA)
    expect_error(quantiles_arg <- analysis_wrapper(quantile, probs = CI_levels), NA)
    expect_error(quantiles_unnamed <- analysis_wrapper(quantile, c(0.05, 0.95)), NA)
    
    # check results
    expect_error(output <- quantiles_default(data), NA)
    expect_equal(dim(output$results[[1]]), c(5, 12))
    expect_identical(output$dataset, "data")
    expect_identical(output$method, "quantile")
    expect_identical(output$args[[1]], list())
    expect_known_hash(digest::digest(output), "bd96f20f5c")
    
    # check results
    expect_error(output <- quantiles_named(data), NA)
    expect_equal(dim(output$results[[1]]), c(2, 12))
    expect_identical(output$dataset, "data")
    expect_identical(output$method, "quantile")
    expect_identical(output$args[[1]], list(probs = c(0.05, 0.95)))
    expect_known_hash(digest::digest(output), "efe506a07c")
    
    # check results
    expect_error(output <- quantiles_arg(data), NA)
    expect_equal(dim(output$results[[1]]), c(2, 12))
    expect_identical(output$dataset, "data")
    expect_identical(output$method, "quantile")
    expect_identical(output$args[[1]], list(probs = c(0.05, 0.95)))
    expect_known_hash(digest::digest(output), "efe506a07c")
    
    # check results
    expect_error(output <- quantiles_unnamed(data), NA)
    expect_equal(dim(output$results[[1]]), c(2, 12))
    expect_identical(output$dataset, "data")
    expect_identical(output$method, "quantile")
    expect_identical(output$args[[1]], list(c(0.05, 0.95)))
    expect_known_hash(digest::digest(output), "1452db2bbb")
})