context("Check Datasets that are installed in the subsampled folder")

path <- system.file("extdata", "subsampled",
                    package = "MATSS", mustWork = TRUE)
Sys.setenv(MATSS_DATA_PATH = path)

test_that("process_bbs_ts_data formats data correctly", {
    unlink(file.path(path, "breed-bird-survey-prepped"))
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
    expect_error(dat <- get_mtquad_data(), NA)
    expect_true(check_data_format(dat))
    expect_known_hash(dat$abundance, "c4a22592f9")
    expect_known_hash(dat$covariates, "f9debd76c0")
    expect_known_hash(dat$metadata, "3a6bbbf578")
    expect_known_hash(dat, "c410abab6a")
})

test_that("get_gpdd_data formats data correctly", {
    expect_error(dat <- get_gpdd_data(location_id = 83, timeperiod_id = 408), NA)
    expect_true(check_data_format(dat))
    expect_known_hash(dat$abundance, "660347d200")
    expect_known_hash(dat$covariates, "808aa40baf")
    expect_known_hash(dat$metadata, "205634668e")
    expect_known_hash(dat, "b1890b0735")
})

test_that("get_biotime_data formats data correctly", {
    unlink(file.path(path, "biotime-prepped"))
    expect_error(w <- capture_warnings(prepare_biotime_data(data_subset = 172)), NA)
    expect_match(w, "All formats failed to parse. No formats found.")
    expect_error(dat <- get_biotime_data(dataset = 321), NA)
    expect_true(check_data_format(dat))
    expect_known_hash(dat$abundance, "3c7a4f434f")
    expect_known_hash(dat$covariates, "bb6df9ff3e")
    expect_known_hash(dat$metadata, "ff95e3e4a6")
    expect_known_hash(dat, "30d7359c31")
})
