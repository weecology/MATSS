context("Check Datasets")

test_path <- tempdir()
Sys.setenv(MATSS_DATA_PATH = test_path)

test_that("get_gpdd_data formats data correctly", {
    skip_if_no_retriever()
    download_datasets("global-population-dynamics")
    
    expect_error(dat <- get_gpdd_data(location_id = 83, timeperiod_id = 408), NA)
    expect_dataset(dat, "701d60bb9e", "303e5d422b", "22f70476be", "9c44940787")
    expect_known_hash(dat$metadata$citation, "6c7d2e15e6")
})

test_that("Shortgrass Steppe data is retrievable and works", {
    skip_if_no_retriever()
    download_datasets("shortgrass-steppe-lter")
    
    expect_error(dat <- get_sgs_data(), NA)
    expect_dataset(dat, "30f412e387", "e87060f72a", "f212abd4ab", "67ba6e0b35")
    expect_known_hash(dat$metadata$citation, "be3383c603")
})

test_that("Jornada data is retrievable and works", {
    skip_if_no_retriever()
    download_datasets("jornada-lter-rodent")
    
    expect_error(dat <- get_jornada_data(), NA)
    expect_dataset(dat, "b01c0f0361", "b71bd81c62", "e13443e3ca", "cdef5e0eb9")
    expect_known_hash(dat$metadata$citation, "ecc667faa2")
})

test_that("Portal data is retrievable and works", {
    expect_error(dat <- get_portal_rodents(), NA)
    expect_dataset(dat, "2136da689d", "ec4befd3dd", "6074e384c2", "0635765f20")
    expect_known_hash(dat$metadata$citation, "85f8b00f1d")
})

test_that("Karoo data is retrievable and works", {
    expect_error(dat <- get_karoo_data(), NA)
    expect_dataset(dat, "811613052a", "72deba00b8", "625990a0b8", "7f959c373d")
    expect_known_hash(dat$metadata$citation, "b54523a903")
})

test_that("Cowley Lizards data is retrievable and works", {
    expect_error(dat <- get_cowley_lizards(), NA)
    expect_dataset(dat, "baf0ca42d4", "36181d12d0", "b59b5ae6b4", "c3f66160ae")
    expect_known_hash(dat$metadata$citation, "e7bbe85cb4")
})

test_that("Cowley Snakes data is retrievable and works", {
    expect_error(dat <- get_cowley_snakes(), NA)
    expect_dataset(dat, "f9d6848c03", "36181d12d0", "b59b5ae6b4", "8ee7798b91")
    expect_known_hash(dat$metadata$citation, "e7bbe85cb4")
})

test_that("Kruger data is retrievable and works", {
    expect_error(dat <- get_kruger_data(), NA)
    expect_dataset(dat, "3184bfcfa6", "e00ef454e1", "4b1f4de879", "7f959c373d")
    expect_known_hash(dat$metadata$citation, "3595d0fbba")
})
