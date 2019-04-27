context("Check Retriever interface functions")

test_path <- tempdir()
test_that("data_path functions work", {
    Sys.setenv(MATSS_DATA_PATH = test_path)
    expect_equal(get_default_data_path(), test_path)
    expect_true(check_default_data_path())
    
    Sys.unsetenv("MATSS_DATA_PATH")
    expect_false(check_default_data_path())
})

# check if `retriever` is installed
skip_if_no_retriever <- function() {
    have_retriever <- reticulate::py_module_available("retriever")
    if (!have_retriever)
        skip("retriever not available for testing")
}

test_that("retriever downloading and importing work", {
    skip_if_no_retriever()
    Sys.setenv(MATSS_DATA_PATH = test_path)
    expect_equal(get_default_data_path(), test_path)
    
    expect_error(install_retriever_data("veg-plots-sdl"), NA)
    expect_error(dat <- import_retriever_data("veg-plots-sdl"), NA)
    expect_known_hash(dat$veg_plots_sdl_Count1906, "4f6b34f60a")
    expect_known_hash(is.na(dat$veg_plots_sdl_Photo_info), "fa7b429803")
    dat$veg_plots_sdl_Photo_info[is.na(dat$veg_plots_sdl_Photo_info)] <- -999999
    expect_known_hash(dat$veg_plots_sdl_Photo_info, "23666fa87a")
    expect_known_hash(dat$veg_plots_sdl_Plot_corners, "53e7e72ee9")
    expect_known_hash(dat$veg_plots_sdl_Plots, "b0c6ec4b6e")
    expect_known_hash(dat$veg_plots_sdl_Seedling_counts, "dd0d14c55a")
    expect_known_hash(dat$veg_plots_sdl_SMCover, "6b03c46cbf")
    expect_known_hash(dat$veg_plots_sdl_SMDensity, "1a5673f8ff")
    expect_known_hash(dat$veg_plots_sdl_Stake_info, "6e7608632d")
    expect_known_hash(dat$veg_plots_sdl_Species, "e4276e9361")
})
