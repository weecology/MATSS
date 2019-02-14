context("test datasets")

test_that("Portal data is retrievable and works", {
    expect_error(portal_data <- get_portal_rodents(), NA)
    expect_true(check_data_format(portal_data))
})

test_that("Jornada data is retrievable and works", {
    expect_error(jornada_data <- get_jornada_data(), NA)
    expect_true(check_data_format(jornada_data))
})

test_that("Shortgrass Steppe data is retrievable and works", {
    expect_error(sgs_data <- get_sgs_data(), NA)
    expect_true(check_data_format(sgs_data))
})
