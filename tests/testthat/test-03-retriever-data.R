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
    
    expect_known_hash(dat$veg_plots_sdl_Count1906, "daf4dd1faa")
    expect_known_hash(dat$veg_plots_sdl_Photo_info, "a6cb447edc")
    expect_known_hash(dat$veg_plots_sdl_Plot_corners, "5174728afa")
    expect_known_hash(dat$veg_plots_sdl_Plots, "3edffbf85a")
    expect_known_hash(dat$veg_plots_sdl_Seedling_counts, "424b7bbfc3")
    expect_known_hash(dat$veg_plots_sdl_SMCover, "911b8ab006")
    expect_known_hash(dat$veg_plots_sdl_SMDensity, "4baa4ce118")
})
