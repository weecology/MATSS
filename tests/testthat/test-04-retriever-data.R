context("Check Retriever datasets")
# 
# test_that("get_bbs_data formats data correctly", {
#     data_path <- system.file("extdata", "subsampled",
#                              package = "MATSS", mustWork = TRUE)
#     Sys.setenv(MATSS_DATA_PATH = data_path)
#     expect_error(dat <- get_bbs_data(region = 4, min_num_yrs = 1), NA)
#     expect_true(check_data_format(dat))
#     expect_known_hash(dat$abundance, "0d083ef430")
#     expect_known_hash(which(is.na(dat$covariates)), "40f5faaa59")
#     dat$covariates[is.na(dat$covariates)] <- -999999
#     expect_known_hash(na.omit(dat$covariates), "6c2f809118")
#     expect_known_hash(dat$metadata, "b5adce4593")
#     expect_known_hash(dat, "72aa13aef2")
# })

test_that("get_mtquad_data formats data correctly", {
    data_path <- system.file("extdata", "subsampled",
                             package = "MATSS", mustWork = TRUE)
    Sys.setenv(MATSS_DATA_PATH = data_path)
    expect_error(dat <- get_mtquad_data(), NA)
    expect_true(check_data_format(dat))
    expect_known_hash(dat$abundance, "c4a22592f9")
    expect_known_hash(dat$covariates, "f9debd76c0")
    expect_known_hash(dat$metadata, "3a6bbbf578")
    expect_known_hash(dat, "c410abab6a")
})

