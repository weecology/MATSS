context("Check Datasets")

test_path <- tempdir()
Sys.setenv(MATSS_DATA_PATH = test_path)

test_that("get_gpdd_data formats data correctly", {
    skip_if_no_retriever()
    download_datasets("global-population-dynamics")
    
    expect_error(dat <- get_gpdd_data(location_id = 83, timeperiod_id = 408), NA)
    expect_dataset(dat, "701d60bb9e", "303e5d422b", "be759de22a")
})

test_that("Shortgrass Steppe data is retrievable and works", {
    skip_if_no_retriever()
    download_datasets("shortgrass-steppe-lter")
    
    expect_error(dat <- get_sgs_data(), NA)
    expect_dataset(dat, "30f412e387", "e87060f72a", "995358babb")
})

test_that("Jornada data is retrievable and works", {
    skip_if_no_retriever()
    download_datasets("jornada-lter-rodent")
    
    expect_error(dat <- get_jornada_data(), NA)
    expect_dataset(dat, "b01c0f0361", "b71bd81c62", "d33ab01da5")
})

test_that("Portal data is retrievable and works", {
    expect_error(dat <- get_portal_rodents(), NA)
    expect_dataset(dat, "2136da689d", "ec4befd3dd", "c6f671dfb1")
})

test_that("Karoo data is retrievable and works", {
    expect_error(dat <- get_karoo_data(), NA)
    expect_dataset(dat, "811613052a", "72deba00b8", "d61d648bc8")
})

test_that("Cowley Lizards data is retrievable and works", {
    expect_error(dat <- get_cowley_lizards(), NA)
    expect_dataset(dat, "baf0ca42d4", "36181d12d0", "8866c9b56b")
})

test_that("Cowley Snakes data is retrievable and works", {
    expect_error(dat <- get_cowley_snakes(), NA)
    expect_dataset(dat, "f9d6848c03", "36181d12d0", "8866c9b56b")
})

test_that("Kruger data is retrievable and works", {
    expect_error(dat <- get_kruger_data(), NA)
    expect_dataset(dat, "3184bfcfa6", "e00ef454e1", "d86941cf7e")
})
