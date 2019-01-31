context("test datasets")

test_that("Portal data is retrievable and works", {
    expect_error(portal_data_raw <- get_portal_rodents(), 
                 NA)
    expect_error(portal_data <- list(abundance = dplyr::select(portal_data_raw, -period, -censusdate), 
                                     covariates = dplyr::select(portal_data_raw, period, censusdate)),
                 NA)
    expect_true(check_data_format(portal_data))
})

test_that("Jornada data is retrievable and works", {
    expect_error(jornada_data = process_jornada_data(), 
                 NA)
    expect_true(check_data_format(jornada_data))
})

test_that("Shortgrass Steppe data is retrievable and works", {
    expect_error(sgs_data = process_sgs_data(), 
                 NA)
    expect_true(check_data_format(sgs_data))
})