context("Time Series Summary Statistics")

test_that("ts_summary error checking works", {
    ts <- sunspot.year
    ts[c(1, 5, 10:14)] <- NA
    expect_error(ts_summary(ts, obs_per_effort = 2), "`obs_per_effort` must be logical")
    expect_message(ts_summary(ts, obs_per_effort = TRUE), "`effort` is `NULL`, assuming all effort = 1")
    expect_error(ts_summary(ts, effort = 1:5), "`obs` and `effort` are not of same length")
})

test_that("ts_summary works", {
    dat <- get_jornada_data()
    obs <- dat$abundance
    n <- NCOL(obs)
    var_names <- names(obs)
    
    # check message output
    expect_error(m <- capture_messages(output <- ts_summary(obs)), NA)
    expect_match(m, "`time` is `NULL`, assuming evenly spaced data", all = FALSE)
    expect_match(m, "`effort` is `NULL`, assuming all effort = 1", all = FALSE)

    # check basic output structure
    expect_true(all(c("num_spp", "num_obs", "stats", "spp_correlations") %in% names(output)))
    expect_equal(output$num_spp, n)
    expect_equal(output$num_obs, NROW(obs))
    
    # check output stats
    expect_equal(dim(output$stats), c(n + 4, 8))
    expect_true(all(c("variable", "min", "max", "median", "mean", "sd", "n", "autocorrelation") 
                    %in% names(output$stats)))
    expect_equal(vapply(output$stats$autocorrelation, length, 0, USE.NAMES = FALSE), 
                 rep.int(14, n + 4))
    expect_true(all(c(var_names, "times", "effort", "richness", "tot_obs")
                    %in% output$stats$variable))
    expect_known_hash(output$stats, "6e8e178b2e")
    
    # check output spp correlations
    expect_equal(dim(output$spp_correlations), c(n, n))
    expect_equal(var_names, rownames(output$spp_correlations))
    expect_equal(var_names, colnames(output$spp_correlations))
    expect_known_hash(output$spp_correlations, "c52fce46d7")
})

test_that("ts_summary works for formatted data", {
    dat <- get_jornada_data()
    n <- NCOL(dat$abundance)
    var_names <- names(dat$abundance)

    # check message output
    expect_error(m <- capture_messages(output <- ts_summary(dat)), NA)
    expect_match(m, "`effort` is `NULL`, assuming all effort = 1", all = FALSE)
    
    # check basic output structure
    expect_true(all(c("num_spp", "num_obs", "stats", "spp_correlations") %in% names(output)))
    expect_equal(output$num_spp, n)
    expect_equal(output$num_obs, NROW(dat$abundance))
    
    # check output stats
    expect_equal(dim(output$stats), c(n + 4, 8))
    expect_true(all(c("variable", "min", "max", "median", "mean", "sd", "n", "autocorrelation") 
                    %in% names(output$stats)))
    expect_equal(vapply(output$stats$autocorrelation, length, 0, USE.NAMES = FALSE), 
                 rep.int(12, n + 4))
    expect_true(all(c(var_names, "times", "effort", "richness", "tot_obs")
                    %in% output$stats$variable))
    expect_known_hash(output$stats, "9ec672111c")
    
    # check output spp correlations
    expect_equal(dim(output$spp_correlations), c(n, n))
    expect_equal(var_names, rownames(output$spp_correlations))
    expect_equal(var_names, colnames(output$spp_correlations))
    expect_known_hash(output$spp_correlations, "c52fce46d7")
})

test_that("ts_summary works with just a time series", {
    ts <- sunspot.year
    ts[c(1, 5, 10:14)] <- NA
    expect_error(m <- capture_messages(output <- ts_summary(ts)), NA)
    expect_match(m, "`time` is `NULL`, assuming evenly spaced data", all = FALSE)
    expect_match(m, "`effort` is `NULL`, assuming all effort = 1", all = FALSE)
    expect_known_hash(output, "2a499d80f9")
})

test_that("uni_ts_summary works with full obs, times, effort", {
    set.seed(42)
    ts <- sunspot.year
    ts[c(1, 5, 10:14)] <- NA
    times <- as.numeric(time(sunspot.year))
    effort <- sample(10:12, length(times), replace = TRUE)
    expect_error(output <- ts_summary(data = ts, times = times, effort = effort), NA)
    expect_known_hash(output, "49ae975ae0")
})

test_that("summarize_df works", {
    expect_known_hash(summarize_df(iris[, 1:4]), "727949b43d")
    expect_error(output <- summarize_df(iris[, 1:4], lag.max = 7), NA)
    expect_equal(vapply(output$autocorrelation, length, 1, USE.NAMES = FALSE), 
                 rep.int(8, 4))
    expect_known_hash(output, "ee2776fcb2")
})

ts <- sunspot.year
test_that("summarize_vec works", {
    expect_error(summarize_vec(ts, NULL, round_out = 1))
    expect_error(summarize_vec(ts, ts, digits = 2.3))
})

test_that("summarize_vec works", {
    expect_error(output <- summarize_vec(ts), NA)
    expect_known_hash(output, "aa847b7a8d")
    
    obs <- data.frame(sunspot.year = as.numeric(sunspot.year))
    expect_error(output <- summarize_vec(obs), NA)
    expect_known_hash(output, "aa847b7a8d")

    expect_error(output <- summarize_vec(time(ts)), NA)
    expect_known_hash(output, "ca53426f70")

    expect_error(output <- summarize_vec(rep(1, NROW(ts))), NA)
    expect_known_hash(output, "24cd619a6a")
})

test_that("temp_autocor works for different data types", {
    ts <- sunspot.year
    ts[c(1, 5, 10:14)] <- NA
    expect_error(output <- temp_autocor(ts, time(sunspot.year)), NA)
    expect_equal(length(output), 25)
    expect_false(any(is.na(output)))
    expect_known_hash(output, "5842a3ef0f")
    
    expect_error(output <- temp_autocor(as.numeric(ts), time(sunspot.year)), NA)
    expect_known_hash(output, "5842a3ef0f")
    
    expect_error(output <- temp_autocor(matrix(ts), time(sunspot.year)), NA)
    expect_known_hash(output, "5842a3ef0f")
    
    expect_error(output <- temp_autocor(data.frame(x = ts), time(sunspot.year)), NA)
    expect_known_hash(output, "5842a3ef0f")
    
    expect_error(output <- temp_autocor(tibble::tibble(x = ts), time(sunspot.year)), NA)
    expect_known_hash(output, "5842a3ef0f")
})

test_that("richness works", {
    expect_error(richness(NULL))
    expect_error(richness(array(rnorm(12), dim = c(3, 4))))
    expect_error(richness(mtcars))
    expect_error(richness(LETTERS[1:10]))
    ts <- rpois(30, lambda = 2) + 1
    ts[sample(30, 5)] <- 0
    expect_error(output <- richness(ts), NA)
    expect_identical(output, 25L)
})

test_that("interpolate_obs works", {
    ts <- sunspot.year
    missing_idx <- c(1, 5, 10:14)
    ts[missing_idx] <- NA
    expect_error(output <- interpolate_obs(ts, time(sunspot.year)), NA)
    expect_identical(length(output), length(ts))
    expect_false(any(is.na(output)))
    expect_true(all(output[missing_idx] >= min(ts, na.rm = TRUE)))
    expect_true(all(output[missing_idx] <= max(ts, na.rm = TRUE)))
    expect_known_hash(output, "0bd2cccc8e")
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
