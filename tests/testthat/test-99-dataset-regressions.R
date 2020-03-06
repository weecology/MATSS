context("Check Datasets")

test_path <- tempdir()
Sys.setenv(MATSS_DATA_PATH = test_path)

test_that("get_gpdd_data formats data correctly", {
    skip_if_no_retriever()
    download_datasets("global-population-dynamics")
    
    expect_error(dat <- get_gpdd_data(location_id = 83, timeperiod_id = 408), NA)
    expect_dataset(dat, "701d60bb9e", "303e5d422b", "22f70476be", "9c44940787")
})

test_that("Shortgrass Steppe data is retrievable and works", {
    skip_if_no_retriever()
    download_datasets("shortgrass-steppe-lter")
    
    expect_error(dat <- get_sgs_data(), NA)
    expect_dataset(dat, "30f412e387", "e87060f72a", "f212abd4ab", "67ba6e0b35")
})

test_that("Jornada data is retrievable and works", {
    skip_if_no_retriever()
    download_datasets("jornada-lter-rodent")
    
    expect_error(dat <- get_jornada_data(), NA)
    expect_dataset(dat, "b01c0f0361", "b71bd81c62", "e13443e3ca", "cdef5e0eb9")
})

test_that("Portal data is retrievable and works", {
    expect_error(dat <- get_portal_rodents(), NA)
    expect_dataset(dat, "2136da689d", "ec4befd3dd", "6074e384c2", "0635765f20")
})

test_that("Karoo data is retrievable and works", {
    expect_error(dat <- get_karoo_data(), NA)
    expect_dataset(dat, "811613052a", "72deba00b8", "625990a0b8", "7f959c373d")
})

test_that("Cowley Lizards data is retrievable and works", {
    expect_error(dat <- get_cowley_lizards(), NA)
    expect_dataset(dat, "baf0ca42d4", "36181d12d0", "8866c9b56b", "7f959c373d")
})

test_that("Cowley Snakes data is retrievable and works", {
    expect_error(dat <- get_cowley_snakes(), NA)
    expect_dataset(dat, "f9d6848c03", "36181d12d0", "8866c9b56b", "7f959c373d")
})

test_that("Kruger data is retrievable and works", {
    expect_error(dat <- get_kruger_data(), NA)
    expect_dataset(dat, "3184bfcfa6", "e00ef454e1", "4b1f4de879", "7f959c373d")
})
