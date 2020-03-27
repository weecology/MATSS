context("Time Series Summary Statistics")

test_path <- tempdir()
Sys.setenv(MATSS_DATA_PATH = test_path)

test_that("ts_summary works", {
    skip_if_no_retriever()
    download_datasets("jornada-lter-rodent")
    dat <- get_jornada_data()
    expect_true(check_data_format(dat))
    obs <- dat$abundance
    n <- NCOL(obs)
    var_names <- names(obs)
    
    # check message output
    expect_error(m <- capture_messages(output <- ts_summary(obs)), NA)
    expect_match(m, "`time` is `NULL`, assuming evenly spaced data", all = FALSE)
    expect_match(m, "`effort` is `NULL`, assuming all effort = 1", all = FALSE)
    
    # check basic output structure
    expect_true(all(c("num_spp", "num_obs", "stats", "spp_correlations") %in% names(output)))
    expect_equal(output$num_spp[1], n)
    expect_equal(output$num_obs[1], NROW(obs))
    
    # check output stats
    stats <- output$stats[[1]]
    expect_equal(dim(stats), c(n + 4, 8))
    expect_true(all(c("variable", "min", "max", "median", "mean", "sd", "n", "autocorrelation") 
                    %in% names(stats)))
    expect_equal(vapply(stats$autocorrelation, length, 0, USE.NAMES = FALSE), 
                 rep.int(14, n + 4))
    expect_true(all(c(var_names, "times", "effort", "richness", "tot_obs")
                    %in% stats$variable))
    expect_known_hash(stats, "1c69815948")
    
    # check output spp correlations
    cor_matrix <- output$spp_correlations[[1]]
    expect_equal(dim(cor_matrix), c(n, n))
    expect_equal(var_names, rownames(cor_matrix))
    expect_equal(var_names, colnames(cor_matrix))
    expect_known_hash(cor_matrix, "67f4ec1955")
})

test_that("ts_summary works for formatted data", {
    skip_if_no_retriever()
    download_datasets("ushio-maizuru-fish-community")
    
    dat <- get_maizuru_data()
    expect_true(check_data_format(dat))
    n <- NCOL(dat$abundance)
    var_names <- names(dat$abundance)

    # check message output
    expect_error(m <- capture_messages(output <- ts_summary(dat)), NA)
    expect_match(m, "`effort` is `NULL`, assuming all effort = 1", all = FALSE)

    # check basic output structure
    expect_true(all(c("num_spp", "num_obs", "stats", "spp_correlations") %in% names(output)))
    expect_equal(output$num_spp[1], n)
    expect_equal(output$num_obs[1], NROW(dat$abundance))

    # check output stats
    stats <- output$stats[[1]]
    expect_equal(dim(stats), c(n + 4, 8))
    expect_true(all(c("variable", "min", "max", "median", "mean", "sd", "n", "autocorrelation")
                    %in% names(stats)))
    expect_equal(vapply(stats$autocorrelation, length, 0, USE.NAMES = FALSE),
                 rep.int(25, n + 4))
    expect_true(all(c(var_names, "times", "effort", "richness", "tot_obs")
                    %in% stats$variable))
    expect_known_hash(stats, "e77e6193fa")

    # check output spp correlations
    cor_matrix <- output$spp_correlations[[1]]
    expect_equal(dim(cor_matrix), c(n, n))
    expect_equal(var_names, rownames(cor_matrix))
    expect_equal(var_names, colnames(cor_matrix))
    expect_known_hash(cor_matrix, "3061024928")
})

test_that("ts_summary works with just a time series", {
    ts <- sunspot.year
    ts[c(1, 5, 10:14)] <- NA
    expect_error(m <- capture_messages(output <- ts_summary(ts)), NA)
    expect_known_hash(tibble::as_tibble(output), "a38b9118f0")
})

test_that("ts_summary works with full obs, times, effort", {
    set.seed(42)
    ts <- sunspot.year
    ts[c(1, 5, 10:14)] <- NA
    times <- as.numeric(time(sunspot.year))
    effort <- sample(10:12, length(times), replace = TRUE)
    expect_error(output <- ts_summary(data = ts, times = times, effort = effort), NA)
    #    expect_known_hash(output$stats[[1]], "4155ffc4fa")
    expect_known_hash(output$stats[[1]]$autocorrelation, "a5322be834")
    expect_known_hash(output$stats[[1]][, -8], "8173cd7aaae")
    expect_known_hash(output$spp_correlations, "1d6dee4961")
    #    expect_known_hash(tibble::as_tibble(output), "d61f90652f")
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
    expect_known_hash(output, "0d9e559fea")
    
    obs <- data.frame(sunspot.year = as.numeric(sunspot.year))
    expect_error(output <- summarize_vec(obs), NA)
    expect_known_hash(output, "0d9e559fea")
    
    expect_error(output <- summarize_vec(time(ts)), NA)
    expect_known_hash(output, "ae89fe6e71")
    
    expect_error(output <- summarize_vec(rep(1, NROW(ts))), NA)
    expect_known_hash(output, "802c1824cc")
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
