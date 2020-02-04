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
    expect_known_hash(dat$metadata, "08894a47db")
    expect_known_hash(dat, "4eb9f31ac3")
    
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

test_that("get_biotime_data processes data correctly", {
    unlink(file.path(path, "biotime-prepped"), recursive = TRUE)
    expect_error(prepare_biotime_data(data_subset = c(1, 14, 67, 172)), NA)
    biotime_data_tables <- import_retriever_data("biotimesql", path = path)
    expect_error(dat <- process_biotime_dataset(biotime_data_tables, dataset_id = 321), NA)
    expect_known_hash(dat$abundance, "e55c7fdbf0")
    expect_known_hash(dat$covariates, "31d9dbfb67")
    expect_known_hash(dat$metadata, "87d9c2aca3")
    expect_known_hash(dat, "72ffafd798")
    
    expect_error(get_biotime_dataset_ids(do_processing = TRUE), NA)
    expect_error(dat <- get_biotime_data(dataset_id = 321), NA)
    expect_true(check_data_format(dat))
    expect_known_hash(dat$abundance, "e55c7fdbf0")
    expect_known_hash(dat$covariates, "31d9dbfb67")
    expect_known_hash(dat$metadata, "0e778e1fc5")
    expect_known_hash(dat, "77183d6516")
    
    expect_true(check_metadata_species_table(dat))
})
