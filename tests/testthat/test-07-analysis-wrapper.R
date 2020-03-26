context("Tests of Wrapper Functions")

data <- dragons
num_vars <- NCOL(data$abundance)

test_that("analysis_wrapper and invoke work for simple functions", {
    # setup sample inputs
    f <- function(ts) {
        tibble::tibble(n = NROW(ts), 
                       mean = mean(ts), 
                       sd = sd(ts))
    }
    
    # check if analysis_wrapper works properly and has the right size
    expect_error(fun <- analysis_wrapper(f), NA)
    expect_error(output <- fun(data), NA)
    expect_equal(dim(output), c(num_vars, 4))
    expect_identical(output$id, names(data$abundance))
    
    # check if invoke works properly
    expect_error(output <- invoke(fun, data), NA)
    expect_equal(dim(output), c(1, 5))
    expect_identical(output$dataset, "data")
    expect_identical(output$method, "fun")
    expect_identical(output$args[[1]], list())
    
    # check metadata
    expect_identical(output$metadata[[1]], data$metadata)
    
    # check digest
    output$metadata[[1]] <- list()
    expect_known_hash(output, "4bd05cfb68")
})

test_that("invoke preserves arguments correctly", {
    # setup sample inputs
    CI_levels <- c(0.05, 0.95)

    # create our different methods
    expect_error(compute_quantiles <- analysis_wrapper(quantile), NA)

    # check results
    expect_error(output <- invoke(compute_quantiles, data), NA)
    expect_equal(dim(output$results[[1]]), c(num_vars, 6))
    expect_known_hash(output$results[[1]], "29da5e7ffc")
    expect_identical(output$dataset, "data")
    expect_identical(output$method, "compute_quantiles")
    expect_identical(output$args[[1]], list())
    
    # check results
    expect_error(output <- invoke(compute_quantiles, data, probs = c(0.05, 0.95)), NA)
    expect_equal(dim(output$results[[1]]), c(num_vars, 3))
    expect_known_hash(output$results[[1]], "5758611722")
    expect_identical(output$dataset, "data")
    expect_identical(output$method, "compute_quantiles")
    expect_identical(output$args[[1]], list(probs = c(0.05, 0.95)))

    # check results
    expect_error(output_alt <- invoke(compute_quantiles, data, probs = CI_levels), NA)
    expect_identical(output_alt, output)

    # check results
    expect_error(output <- invoke(compute_quantiles, data, c(0.05, 0.95)), NA)
    expect_equal(dim(output$results[[1]]), c(num_vars, 3))
    expect_known_hash(output$results[[1]], "5758611722")
    expect_identical(output$dataset, "data")
    expect_identical(output$method, "compute_quantiles")
    expect_identical(output$args[[1]], list(c(0.05, 0.95)))
})