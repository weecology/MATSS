context("Time Series Summary Statistics")

test_that("interpolate_obs works", {
    # input check
    expect_error(interpolate_obs(mtcars, seq_len(NROW(mtcars))))
    expect_error(interpolate_obs(matrix(rnorm(16), nrow = 4), 1:10))
    expect_error(interpolate_obs(sunspot.year, NA))
    expect_error(interpolate_obs(sunspot.year, mtcars))
    expect_error(interpolate_obs(sunspot.year, time(sunspot.year), NA))
    
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

