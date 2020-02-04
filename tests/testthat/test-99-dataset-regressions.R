context("Check Datasets")

test_that("get_gpdd_data formats data correctly", {
    skip_if_no_retriever()
    test_path <- tempdir()
    Sys.setenv(MATSS_DATA_PATH = test_path)
    download_datasets("global-population-dynamics")
    
    expect_error(dat <- get_gpdd_data(location_id = 83, timeperiod_id = 408), NA)
    expect_true(check_data_format(dat))
    expect_known_hash(dat$abundance, "701d60bb9e")
    expect_known_hash(dat$covariates, "303e5d422b")
    expect_known_hash(dat$metadata, "47e011df79")
    expect_known_hash(dat, "fbb6ce96f3")
})

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
    expect_known_hash(jornada_data, "ac97408fcd")
})

test_that("Shortgrass Steppe data is retrievable and works", {
    expect_error(sgs_data <- get_sgs_data(), NA)
    expect_true(check_data_format(sgs_data))
    expect_known_hash(sgs_data, "014b440328")
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
