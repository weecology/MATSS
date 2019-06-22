context("Check Retriever datasets")

# check if `retriever` is installed
skip_if_no_retriever <- function()
{
    have_retriever <- reticulate::py_module_available("retriever")
    if (!have_retriever)
        skip("retriever not available for testing")
}

test_that("veg-plots-sdl data retrieval works correctly", {
    skip_if_no_retriever()
    test_path <- tempdir()
    Sys.setenv(MATSS_DATA_PATH = test_path)
    
    expect_error(install_retriever_data("veg-plots-sdl"), NA)
    expect_error(dat <- import_retriever_data("veg-plots-sdl"), NA)
    expect_false(is.null(dat))
    
    expect_error(sdl_data <- get_sdl_data(), NA)
    expect_true(check_data_format(sdl_data))
    expect_known_hash(sdl_data, "e61d747927")
})
