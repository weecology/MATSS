context("Tests of Wrapper Functions")

data <- get_sgs_data()
num_vars <- NCOL(data$abundance)

test_that("analysis_wrapper works for simple functions", {
    # setup sample inputs
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
    expect_equal(dim(results), c(num_vars, 4))
    expect_identical(results$id, names(data$abundance))
    expect_identical(output$dataset, "data")
    expect_identical(output$method, "fun")
    expect_identical(output$args[[1]], list())
    
    # check metadata
    expect_identical(output$metadata[[1]], data$metadata)
    
    # check digest
    expect_known_hash(output, "f448983294")
})

test_that("analysis_wrapper preserves arguments correctly", {
    # setup sample inputs
    CI_levels <- c(0.05, 0.95)

    # create our different methods
    expect_error(quantiles_default <- analysis_wrapper(quantile), NA)
    expect_error(quantiles_named <- analysis_wrapper(quantile, probs = c(0.05, 0.95)), NA)
    expect_error(quantiles_arg <- analysis_wrapper(quantile, probs = CI_levels), NA)
    expect_error(quantiles_unnamed <- analysis_wrapper(quantile, c(0.05, 0.95)), NA)
    
    # check results
    expect_error(output <- quantiles_default(data), NA)
    expect_equal(dim(output$results[[1]]), c(num_vars, 6))
    expect_true("id" %in% names(output$results[[1]]))
    expect_identical(output$dataset, "data")
    expect_identical(output$method, "quantile")
    expect_identical(output$args[[1]], list())
    expect_known_hash(output, "510d26ec15")
    
    # check results
    expect_error(output <- quantiles_named(data), NA)
    expect_equal(dim(output$results[[1]]), c(num_vars, 3))
    expect_true("id" %in% names(output$results[[1]]))
    expect_identical(output$dataset, "data")
    expect_identical(output$method, "quantile")
    expect_identical(output$args[[1]], list(probs = c(0.05, 0.95)))
    expect_known_hash(output, "b6850f1cad")
    
    # check results
    expect_error(output <- quantiles_arg(data), NA)
    expect_equal(dim(output$results[[1]]), c(num_vars, 3))
    expect_true("id" %in% names(output$results[[1]]))
    expect_identical(output$dataset, "data")
    expect_identical(output$method, "quantile")
    expect_identical(output$args[[1]], list(probs = c(0.05, 0.95)))
    expect_known_hash(output, "b6850f1cad")
    
    # check results
    expect_error(output <- quantiles_unnamed(data), NA)
    expect_equal(dim(output$results[[1]]), c(num_vars, 3))
    expect_true("id" %in% names(output$results[[1]]))
    expect_identical(output$dataset, "data")
    expect_identical(output$method, "quantile")
    expect_identical(output$args[[1]], list(c(0.05, 0.95)))
    expect_known_hash(output, "4b876cd612")
})