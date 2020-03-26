context("Building Drake Plans")

test_that("build_datasets_plan works", {
    expect_error(datasets <- build_datasets_plan(), NA)
    expect_plan(datasets)
    expect_equal(dim(datasets), c(5, 2))
    
    expect_error(datasets <- build_datasets_plan(include_retriever_data = TRUE), NA)
    expect_plan(datasets)
    expect_equal(dim(datasets), c(10, 3))
})

test_that("build_analyses_plan works", {
    datasets <- build_datasets_plan()
    methods <- drake::drake_plan(
        abs = abs, 
        mean = mean
    )
    N <- NROW(datasets)
    M <- NROW(methods)
    
    expect_error(analyses <- build_analyses_plan(methods, datasets), NA)
    expect_equal(NROW(analyses), N * M + M)
    
    expect_error(analyses <- build_analyses_plan(methods, datasets, trace = TRUE), NA)
    expect_equal(NROW(analyses), N * M + M)
    
    subplan_abs <- dplyr::filter(analyses, grepl("^analysis_abs_", target))
    expect_equal(NROW(subplan_abs), N)
    expect_true(all(subplan_abs$fun == "abs"))
    expect_identical(subplan_abs$data, datasets$target)
    
    subplan_mean <- dplyr::filter(analyses, grepl("^analysis_mean_", target))
    expect_equal(NROW(subplan_mean), N)
    expect_true(all(subplan_mean$fun == "mean"))
    expect_identical(subplan_mean$data, datasets$target)
    
    subplan_results <- dplyr::filter(analyses, grepl("^results_", target))
    expect_equal(NROW(subplan_results), M)
    expect_identical(subplan_results$fun, methods$target)
    
    fun_calls <- lapply(subplan_results$command, as.character)
    expect_true(all(vapply(fun_calls, dplyr::first, "") == "dplyr::bind_rows"))
})
