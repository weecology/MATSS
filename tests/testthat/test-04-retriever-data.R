context("Check Retriever datasets")

test_that("veg-plots-sdl data retrieval works correctly", {
    skip_if_no_retriever()
    test_path <- tempdir()
    Sys.setenv(MATSS_DATA_PATH = test_path)
    
    expect_error(install_retriever_data("veg-plots-sdl"), NA)
    expect_error(dat <- import_retriever_data("veg-plots-sdl"), NA)
    expect_false(is.null(dat))
    
    expect_error(sdl_data <- get_sdl_data(), NA)
    expect_true(check_data_format(sdl_data))
    sdl_data$metadata$citation <- NULL
    
    expect_known_hash(sdl_data, "1d2cfd50a7")
})
