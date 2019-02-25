context("Time Series Summary Statistics")

test_that("check_obs works", {
    expect_error(check_obs(NULL))
    expect_error(check_obs(mtcars))
    expect_error(check_obs(matrix(rnorm(16), nrow = 4)))
    expect_error(check_obs(matrix(rnorm(16), nrow = 4), single_dim_obs = FALSE), NA)
    expect_error(check_obs(c(NA, rnorm(16))), NA)
})

test_that("check_times works", {
    expect_error(check_times(NULL))
    expect_error(check_times(mtcars))
    expect_error(check_times(time(sunspot.year)), NA)
    expect_error(check_times(c(NA, time(sunspot.year))), NA)
    expect_error(check_times(rnorm(16)), NA)
    expect_error(check_times(c(NA, rnorm(16))), NA)
})


test_that("temp_autocor works", {
    # output check
    ts <- sunspot.year
    ts[c(1, 5, 10:14)] <- NA
    expect_error(output <- temp_autocor(ts, time(sunspot.year)), NA)
    expect_identical(length(output), length(ts))
    expect_false(any(is.na(output)))
    
    # regression check
    expect_identical(digest::digest(output), 
                     "0bd2cccc8efd598126314105fc5552ae")
})

test_that("interpolate_obs works", {
    # output check
    ts <- sunspot.year
    ts[c(1, 5, 10:14)] <- NA
    expect_error(output <- interpolate_obs(ts, time(sunspot.year)), NA)
    expect_identical(length(output), length(ts))
    expect_false(any(is.na(output)))
    
    # regression check
    expect_identical(digest::digest(output), 
                     "0bd2cccc8efd598126314105fc5552ae")
})

