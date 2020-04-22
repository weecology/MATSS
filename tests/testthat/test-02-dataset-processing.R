context("Check Dataset Processing Code")

test_that("BBS data processing works", {
    species_table <- data.frame(aou = c(1, 3, 33, 44), 
                                spanish_common_name = c("a x", "c x", "c x / yy", "d x zz"))
    
    bbs_data_table <- data.frame(species_id = c(1, 2, 3, 33, 44), 
                                 abundance = c(2, 2, 2, 2, 2))
    
    expect_error(out <- combine_bbs_subspecies(bbs_data_table, species_table), NA)
    expect_equal(dim(out), c(4, 2))
    expect_false(any(is.na(out)))
    expect_equal(out$species_id, c(1, 2, 3, 44))
    expect_equal(out$abundance, c(2, 2, 4, 2))
})

sample_dat <- list(
    abundance = data.frame(a = c(1, 2, 3, 5, 6, 9, 10, 11, 12, 13), 
                           b = c(1, 1, 1, 1, 1, 1, 1,  1,  1,  1)), 
    covariates = data.frame(time = c(0, 0.5, 1, 2, 2.5, 4, 4.5, 5, 5.5, 6)), 
    metadata = list(timename = "time", 
                    period = 0.5, 
                    is_community = FALSE, 
                    citation = "NA")
)
attr(sample_dat, "class") <- "matssdata"

test_that("get_full_times works", {
    expect_error(full_times <- get_full_times(sample_dat), NA)
    expect_equal(full_times, seq(0.0, 6.0, by = 0.5))
    
    error_dat <- sample_dat
    error_dat$metadata$timename <- NULL
    expect_error(get_full_times(error_dat))
})

test_that("is_equitimed works", {
    m <- capture_messages(expect_false(is_equitimed(dragons)))
    expect_match(m, "`x` is not a regular sequence.", fixed = TRUE)
    
    path <- system.file("extdata", "subsampled",
                        package = "MATSS", mustWork = TRUE)
    dat <- get_mtquad_data(path = file.path(path, "mapped-plant-quads-mt"))
    m <- capture_messages(expect_true(is_equitimed(dat)))
    expect_match(m, "No time period found. Assuming period = 1.", fixed = TRUE)
})

test_that("make_equitimed works", {
    expect_error(make_equitimed(dragons), 
                 "Unable to construct an evenly spaced time index.", 
                 fixed = TRUE)

    path <- system.file("extdata", "subsampled",
                        package = "MATSS", mustWork = TRUE)
    dat <- get_mtquad_data(path = file.path(path, "mapped-plant-quads-mt"))
    expect_error(m <- capture_messages(out <- make_equitimed(dat)), NA)
    expect_match(m, "No time period found. Assuming period = 1.", fixed = TRUE, all = FALSE)
    expect_match(m, "Dataset is already evenly sampled in time.", fixed = TRUE, all = FALSE)
    expect_equal(out, dat)
    
    expect_error(out <- make_equitimed(sample_dat), NA)
    expect_true(is_equitimed(out))
    expect_true(check_data_format(out))
    expect_equal(dim(out$abundance), c(13, 2))
    expect_true(all(is.na(out$abundance[c(4, 7, 8), ])))
    expect_true(!any(is.na(out$abundance[-c(4, 7, 8), ])))
})



test_that("make_integer_times", {
    expect_error(out <- make_integer_times(sample_dat), 
                 "Dataset is not evenly sampled in time.")
    
    expect_error(out <- make_integer_times(make_equitimed(sample_dat)), NA)
    expect_true(check_data_format(out))
    expect_equal(dim(out$covariates), c(13, 2))
    expect_equal(out$covariates[[2]], 1:13)

    path <- system.file("extdata", "subsampled",
                        package = "MATSS", mustWork = TRUE)
    dat <- get_mtquad_data(path = file.path(path, "mapped-plant-quads-mt"))
    m <- capture_messages(out <- make_integer_times(dat))
    expect_match(m, "No time period found. Assuming period = 1.", fixed = TRUE, all = FALSE)
    expect_match(m, "Dataset appears evenly sampled in time with integer times already.", fixed = TRUE, all = FALSE)
    expect_equal(out, dat)
})