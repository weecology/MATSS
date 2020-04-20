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

test_that("is_equitimed works", {
    m <- capture_error(expect_false(is_equitimed(dragons)))
    expect_match(as.character(m), "Error: `x` is not a regular sequence.", fixed = TRUE)
    
    path <- system.file("extdata", "subsampled",
                        package = "MATSS", mustWork = TRUE)
    dat <- get_mtquad_data()
    expect_true(is_equitimed(dat))
})





test_that("make_integer_times")