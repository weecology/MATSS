context("Check Retriever datasets")

test_that("process_bbs_ts_data formats data correctly", {
    data_path <- system.file("extdata", "subsampled",
                             package = "MATSS", mustWork = TRUE)
    Sys.setenv(MATSS_DATA_PATH = data_path)
    expect_error(prepare_bbs_ts_data(), NA)
    expect_error(dat <- get_bbs_route_region_data(route = 1, region = 11), NA)
    expect_true(check_data_format(dat))
    expect_known_hash(dat$abundance, "3fe07b68b9")
    expect_known_hash(dat$covariates, "3854304cf6")
    expect_known_hash(dat$metadata, "f50efbadbf")
    expect_known_hash(dat, "05adc99ddf")
    
    expect_error(dat <- get_bbs_route_region_data(route = 2, region = 11), NA)
    expect_true(check_data_format(dat))
    expect_error(dat <- get_bbs_route_region_data(route = 3, region = 11), NA)
    expect_true(check_data_format(dat))
})

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

