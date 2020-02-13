context("Check Datasets")

test_path <- tempdir()
Sys.setenv(MATSS_DATA_PATH = test_path)

test_that("get_gpdd_data formats data correctly", {
    skip_if_no_retriever()
    download_datasets("global-population-dynamics")
    
    expect_error(dat <- get_gpdd_data(location_id = 83, timeperiod_id = 408), NA)
    expect_true(check_data_format(dat))
    expect_known_hash(dat$abundance, "701d60bb9e")
    expect_known_hash(dat$covariates, "303e5d422b")
    dat$metadata$citation[2] <- ""
    expect_known_hash(dat$metadata, "5afdbdd6ba")
    expect_known_hash(dat, "95a131b00e")
})

test_that("Shortgrass Steppe data is retrievable and works", {
    skip_if_no_retriever()
    download_datasets("shortgrass-steppe-lter")
    
    expect_error(sgs_data <- get_sgs_data(), NA)
    expect_true(check_data_format(sgs_data))
    expect_known_hash(sgs_data$abundance, "30f412e387")
    expect_known_hash(sgs_data$covariates, "e87060f72a")
    sgs_data$metadata$citation <- NULL
    expect_known_hash(sgs_data$metadata, "5b4f0b2733")
    expect_known_hash(sgs_data, "ddee7094eb")
})

test_that("Portal data is retrievable and works", {
    expect_error(portal_data <- get_portal_rodents(), NA)
    expect_true(check_data_format(portal_data))
    expect_known_hash(portal_data$abundance, "2136da689d")
    expect_known_hash(portal_data$covariates, "ec4befd3dd")
    portal_data$metadata$citation <- NULL
    expect_known_hash(portal_data$metadata, "243346ff63")
    expect_known_hash(portal_data, "8ec181b73d")
    
    expect_error(portal_data <- get_portal_rodents("time", 
                                                   "exclosure"), NA)
    expect_true(check_data_format(portal_data))
    expect_known_hash(portal_data$abundance, "d9948b7ad7")
    expect_known_hash(portal_data$covariates, "effaf21489")
    portal_data$metadata$citation <- NULL
    expect_known_hash(portal_data$metadata, "243346ff63")
    expect_known_hash(portal_data, "79a6b18e55")
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
