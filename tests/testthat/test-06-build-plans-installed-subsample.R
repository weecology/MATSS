context("Building Drake Plans for datasets installed in the subsampled folder")

expect_plan <- function(plan)
{
    eval(bquote(expect_true(tibble::is_tibble(.(plan)))))
    eval(bquote(expect_true("drake_plan" %in% class(.(plan)))))
    eval(bquote(expect_true(all(c("target", "command") %in% names(.(plan))))))
    eval(bquote(expect_equal(class(.(plan)$target), "character")))
    eval(bquote(expect_equal(class(.(plan)$command), "list")))
}

path <- system.file("extdata", "subsampled",
                    package = "MATSS", mustWork = TRUE)
Sys.setenv(MATSS_DATA_PATH = path)

test_that("build_bbs_datasets_plan works", {
    expect_error(datasets <- build_bbs_datasets_plan(), NA)
    expect_plan(datasets)
    expect_true(all(grepl("bbs_data_rtrg_[0-9]+_[0-9]+$", datasets$target)))
    expect_equal(dim(datasets), c(3, 3))
    
    expect_error(datasets <- build_datasets_plan(include_retriever_data = TRUE, 
                                                 include_bbs_data = TRUE), NA)
    expect_plan(datasets)
    expect_equal(sum(grepl("_data$", datasets$target)), 10)
    expect_equal(sum(grepl("bbs_data_rtrg_[0-9]+_[0-9]+$", datasets$target)), 3)
    expect_equal(dim(datasets), c(13, 3))
})

test_that("build_gpdd_datasets_plan works", {
    expect_error(datasets <- build_gpdd_datasets_plan(), NA)
    expect_plan(datasets)
    expect_true(all(grepl("gpdd_data_rtrg_[0-9]+_[0-9\\.]+$", datasets$target)))
    expect_equal(dim(datasets), c(120, 2))
    
    expect_error(datasets <- build_datasets_plan(include_gpdd_data = TRUE), NA)
    expect_plan(datasets)
    expect_equal(sum(grepl("_data$", datasets$target)), 7)
    expect_equal(sum(grepl("gpdd_data_rtrg_[0-9]+_[0-9\\.]+$", datasets$target)), 120)
    expect_equal(dim(datasets), c(127, 2))
})

test_that("build_biotime_datasets_plan works", {
    expect_error(datasets <- build_biotime_datasets_plan(do_processing = FALSE), NA)
    expect_plan(datasets)
    expect_true(all(grepl("biotime_data_rtrg_[0-9]+$", datasets$target)))
    expect_equal(dim(datasets), c(361, 3))
    
    expect_error(datasets <- build_datasets_plan(include_biotime_data = TRUE, 
                                                 biotime_process = FALSE), NA)
    expect_plan(datasets)
    expect_equal(sum(grepl("_data$", datasets$target)), 7)
    expect_equal(sum(grepl("biotime_data_rtrg_[0-9]+$", datasets$target)), 361)
    expect_equal(dim(datasets), c(368, 3))
})

# test_that("build_bbs_datasets_plan works", {
#     expect_error(datasets <- build_datasets_plan(include_bbs_data = T), NA)
#     
#     datasets <- build_datasets_plan(include_bbs_data = T)
#     
#     expect_plan(datasets)
#    # expect_true(all(grepl("_data$", datasets$target)))
#     expect_equal(dim(datasets), c(2594, 2))
#     
#     methods <- drake::drake_plan(
#         abs = abs, 
#         mean = mean
#     )
#     N <- NROW(datasets)
#     M <- NROW(methods)
#     
#     expect_error(analyses <- build_analyses_plan(methods, datasets), NA)
#     expect_equal(NROW(analyses), N * M + M)
#     
#     expect_error(analyses <- build_analyses_plan(methods, datasets, trace = TRUE), NA)
#     expect_equal(NROW(analyses), N * M + M)
#     
#     subplan_abs <- dplyr::filter(analyses, grepl("^analysis_abs_", target))
#     expect_equal(NROW(subplan_abs), N)
#     expect_true(all(subplan_abs$fun == "abs"))
#     expect_identical(subplan_abs$data, datasets$target)
#     
#     subplan_mean <- dplyr::filter(analyses, grepl("^analysis_mean_", target))
#     expect_equal(NROW(subplan_mean), N)
#     expect_true(all(subplan_mean$fun == "mean"))
#     expect_identical(subplan_mean$data, datasets$target)
#     
#     subplan_results <- dplyr::filter(analyses, grepl("^results_", target))
#     expect_equal(NROW(subplan_results), M)
#     expect_identical(subplan_results$fun, methods$target)
#     
#     fun_calls <- lapply(subplan_results$command, as.character)
#     expect_true(all(vapply(fun_calls, dplyr::first, "") == "MATSS::collect_analyses"))
#     expect_true(all(grepl("^list\\(", vapply(fun_calls, dplyr::last, ""))))
#     
# })
