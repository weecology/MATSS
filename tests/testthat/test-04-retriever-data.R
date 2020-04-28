context("Check Retriever datasets")

test_that("veg-plots-sdl data retrieval works correctly", {
    skip_if_no_retriever()
    test_path <- tempdir()
    Sys.setenv(MATSS_DATA_PATH = test_path)
    
    expect_error(install_retriever_data("veg-plots-sdl"), NA)
    expect_error(dat <- import_retriever_data("veg-plots-sdl"), NA)
    expect_false(is.null(dat))
    
    expect_error(dat <- get_sdl_data(), NA)
    expect_dataset(dat, "ff06f7af67", "de2dc7f655", "d52a190865", "d98472ffcc")
})
