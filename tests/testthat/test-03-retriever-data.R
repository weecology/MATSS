context("Check Retriever interface functions")

test_path <- tempdir()
test_that("data_path functions work", {
    Sys.setenv(MATSS_DATA_PATH = test_path)
    expect_equal(get_default_data_path(), test_path)
    expect_true(check_default_data_path())
    
    Sys.unsetenv("MATSS_DATA_PATH")
    expect_false(check_default_data_path())
})


test_that("retriever download and importing work", {
    Sys.setenv(MATSS_DATA_PATH = test_path)
    expect_equal(get_default_data_path(), test_path)
    
    expect_error(install_retriever_data("veg-plots-sdl"), NA)
    expect_error(dat <- import_retriever_data("veg-plots-sdl"), NA)
    
    expect_known_hash(dat, "872b0d5be1")
})