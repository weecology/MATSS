context("Check Datasets")

test_that("Portal data is retrievable and works", {
    expect_error(portal_data <- get_portal_rodents(), NA)
    expect_true(check_data_format(portal_data))
    expect_known_hash(portal_data, "8ee69162ed")
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
