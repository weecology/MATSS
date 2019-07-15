context("Building Drake Plans")

expect_plan <- function(plan)
{
    eval(bquote(expect_true(tibble::is_tibble(.(plan)))))
    eval(bquote(expect_true("drake_plan" %in% class(.(plan)))))
    eval(bquote(expect_true(all(c("target", "command") %in% names(.(plan))))))
    eval(bquote(expect_equal(class(.(plan)$target), "character")))
    eval(bquote(expect_equal(class(.(plan)$command), "list")))
}

test_that("collect_analyses helper works", {
    a <- runif(3)
    b <- LETTERS[5:10]
    expect_error(result <- collect_analyses(list(a, b)), NA)
    expect_true(is.list(result))
    expect_identical(names(result), c("a", "b"))
    expect_identical(result$a, a)
    expect_identical(result$b, b)
})

test_that("build_datasets_plan works", {
    expect_error(datasets <- build_datasets_plan(), NA)
    expect_plan(datasets)
    expect_true(all(grepl("_data$", datasets$target)))
    expect_equal(dim(datasets), c(7, 2))
    
    expect_error(datasets <- build_datasets_plan(include_retriever_data = TRUE), NA)
    expect_plan(datasets)
    expect_true(all(grepl("_data$", datasets$target)))
    expect_equal(dim(datasets), c(10, 2))
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
    expect_true(all(vapply(fun_calls, dplyr::first, "") == "MATSS::collect_analyses"))
    expect_true(all(grepl("^list\\(", vapply(fun_calls, dplyr::last, ""))))
})
