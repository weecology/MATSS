context("Check Datasets")

test_that("Portal data is retrievable and works", {
    expect_error(portal_data <- get_portal_rodents(), NA)
    expect_true(check_data_format(portal_data))
    expect_known_hash(portal_data, "e7dfd234a7")

    expect_error(portal_data <- get_portal_rodents("time", 
                                                   "exclosure"), NA)
    expect_true(check_data_format(portal_data))
    expect_known_hash(portal_data, "33df96d995")
})

test_that("Jornada data is retrievable and works", {
    expect_error(jornada_data <- get_jornada_data(), NA)
    expect_true(check_data_format(jornada_data))
    expect_known_hash(jornada_data, "70ce3c4502")
})

test_that("Shortgrass Steppe data is retrievable and works", {
    expect_error(sgs_data <- get_sgs_data(), NA)
    expect_true(check_data_format(sgs_data))
    expect_known_hash(sgs_data, "ccbdc633c1")
})

test_that("Maizuru data is retrievable and works", {
    expect_error(maizuru_data <- get_maizuru_data(), NA)
    expect_true(check_data_format(maizuru_data))
    expect_known_hash(maizuru_data, "c93bd061db")
})

test_that("Karoo data is retrievable and works", {
    expect_error(karoo_data <- get_karoo_data(), NA)
    expect_true(check_data_format(karoo_data))
    expect_known_hash(karoo_data, "d76b9dd7cf")
})

test_that("Cowley Lizards data is retrievable and works", {
    expect_error(cowley_lizards_data <- get_cowley_lizards(), NA)
    expect_true(check_data_format(cowley_lizards_data))
    expect_known_hash(cowley_lizards_data, "790a7d013b")
})

test_that("Cowley Snakes data is retrievable and works", {
    expect_error(cowley_snakes_data <- get_cowley_snakes(), NA)
    expect_true(check_data_format(cowley_snakes_data))
    expect_known_hash(cowley_snakes_data, "1605432bba")
})

test_that("Kruger data is retrievable and works", {
    expect_error(kruger_data <- get_kruger_data(), NA)
    expect_true(check_data_format(kruger_data))
    expect_known_hash(kruger_data, "697241f081")
})

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

test_that("get_gpdd_data formats data correctly", {
    data_path <- system.file("extdata", "subsampled",
                             package = "MATSS", mustWork = TRUE)
    Sys.setenv(MATSS_DATA_PATH = data_path)
    expect_error(dat <- get_gpdd_data(location_id = 83, timeperiod_id = 408), NA)
    expect_true(check_data_format(dat))
    expect_known_hash(dat$abundance, "660347d200")
    expect_known_hash(dat$covariates, "4ce77740e6")
    expect_known_hash(dat$metadata, "205634668e")
    expect_known_hash(dat, "733966e194")
})

test_that("get_biotime_data formats data correctly", {
    data_path <- system.file("extdata", "subsampled",
                             package = "MATSS", mustWork = TRUE)
    Sys.setenv(MATSS_DATA_PATH = data_path)
    expect_error(dat <- get_biotime_data(dataset = 321), NA)
    expect_true(check_data_format(dat))
    expect_known_hash(dat$abundance, "3c7a4f434f")
    expect_known_hash(dat$covariates, "bb6df9ff3e")
    expect_known_hash(dat$metadata, "ff95e3e4a6")
    expect_known_hash(dat, "30d7359c31")
})
