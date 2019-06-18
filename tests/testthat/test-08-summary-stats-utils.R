context("Time Series Summary Statistics Utility Functions")

test_that("normalize_obs works", {
    expect_error(normalize_obs(rnorm(50), obs_per_effort = 1), 
                 "`obs_per_effort` must be logical")
    expect_error(normalize_obs(rnorm(50), effort = 1:10), 
                 "`obs` and `effort` are not of same length")
    x <- 1 + rpois(20, 4)
    expect_error(output <- normalize_obs(x, x), NA)
    expect_equal(output, rep(1, 20))
})

test_that("normalize_times works", {
    m <- capture_messages(expect_equal(normalize_times(rnorm(50)), 1:50))
    expect_match(m, "`time` is `NULL`, assuming evenly spaced data")
    m <- capture_messages(expect_equal(normalize_times(rnorm(50), 
           seq.Date(from = as.Date("2000-01-01"), by = "2 days", length.out = 50)), 1:50))
    expect_match(m, "`time` is not numeric, assuming evenly spaced data")
    expect_equal(normalize_times(rnorm(4), c(1, 1, 2, 3)), c(1, 1, 2, 3))
})

test_that("normalize_effort works", {
    m <- capture_messages(expect_equal(normalize_effort(rnorm(50)), rep(1, 50)))
    expect_match(m, "`effort` is `NULL`, assuming all effort = 1")
    expect_equal(normalize_effort(rnorm(4), c(1, 1, 2, 3)), c(1, 1, 2, 3))
})

test_that("to_numeric_vector works", {
    expect_equal(to_numeric_vector(1:6), 1:6)
    expect_equal(to_numeric_vector(mtcars), mtcars$mpg)
    expect_equal(to_numeric_vector(tibble::as_tibble(mtcars)), mtcars$mpg)
    expect_equal(to_numeric_vector(matrix(1:12, ncol = 2)), 1:6)
})

test_that("check_interp_method works", {
    expect_error(check_interp_method(NULL))
    expect_error(check_interp_method(NA))
    expect_error(check_interp_method(mtcars))
    expect_error(check_interp_method(mean), NA)
    expect_error(check_interp_method(forecast::na.interp), NA)
})

test_that("check_obs works", {
    expect_error(check_obs(NULL))
    expect_error(check_obs(ChickWeight))
    expect_error(check_obs(mtcars), NA)
    expect_error(check_obs(matrix(rnorm(16), nrow = 4)), NA)
    expect_error(check_obs(c(NA, rnorm(16))), NA)
})

test_that("check_effort works", {
    expect_error(check_effort(NULL))
    expect_error(check_effort(mtcars))
    expect_error(check_effort(matrix(rnorm(16), nrow = 4)))
    expect_error(check_effort(c(NA, rnorm(16))), NA)
})

test_that("check_times works", {
    expect_error(check_times(NULL))
    expect_error(check_times(mtcars))
    expect_error(check_times(matrix(rnorm(100), ncol = 10)))
    expect_error(check_times(time(sunspot.year)), NA)
    expect_error(check_times(c(NA, time(sunspot.year))), NA)
    expect_error(check_times(rnorm(16)), NA)
    expect_error(check_times(c(NA, rnorm(16))), NA)
})

test_that("check_obs_and_times works", {
    expect_error(check_obs_and_times(rnorm(16), rnorm(15)))
    expect_error(check_obs_and_times(sunspot.year, time(sunspot.year)), NA)
})
