context("Check Retriever utility functions")

test_path <- tempdir()

test_that("data_path functions work", {
    Sys.unsetenv("MATSS_DATA_PATH")
    expect_false(check_default_data_path())

    Sys.setenv(MATSS_DATA_PATH = test_path)
    expect_equal(get_default_data_path(), test_path)
    expect_true(check_default_data_path())
})

# check if `retriever` is installed
skip_if_no_retriever <- function() {
    have_retriever <- reticulate::py_module_available("retriever")
    if (!have_retriever)
        skip("retriever not available for testing")
}

test_that("retriever installing does error checking", {
    skip_if_no_retriever()
    expect_error(install_retriever_data("veg-plots-sdl"), NA)
    m <- capture_messages(install_retriever_data("veg-plots-sdl"))
    expect_match(m, "A folder already exists for \"veg-plots-sdl\"")
    expect_match(m, "Use `force_install = TRUE` to overwrite it with a fresh install.")
})

test_that("retriever importing does error checking", {
    unused_filename <- tempfile(tmpdir = "")
    unused_filename <- substring(unused_filename, 2, nchar(unused_filename))
    expect_false(file.exists(file.path(test_path, unused_filename)))
    expect_error(w <- capture_warnings(dat <- import_retriever_data(unused_filename)), NA)
    expect_true(is.null(dat))
    expect_match(w, paste0("Didn't find any downloaded data in ", 
                           file.path(test_path, unused_filename)))
    expect_match(w, "Did you run get_retriever_data\\(\\) first?")
})

test_that("retriever importing works", {
    skip_if_no_retriever()
    expect_error(dat <- import_retriever_data("veg-plots-sdl"), NA)
    expect_known_hash(dat$veg_plots_sdl_Count1906, "4f6b34f60a")
    expect_known_hash(which(is.na(dat$veg_plots_sdl_Photo_info)), "834776a184")
    expect_known_hash(na.omit(dat$veg_plots_sdl_Photo_info), "3f10e7e726")
    expect_known_hash(dat$veg_plots_sdl_Plot_corners, "53e7e72ee9")
    expect_known_hash(dat$veg_plots_sdl_Plots, "b0c6ec4b6e")
    expect_known_hash(dat$veg_plots_sdl_Seedling_counts, "dd0d14c55a")
    expect_known_hash(dat$veg_plots_sdl_SMCover, "6b03c46cbf")
    expect_known_hash(dat$veg_plots_sdl_SMDensity, "1a5673f8ff")
    expect_known_hash(dat$veg_plots_sdl_Stake_info, "6e7608632d")
    expect_known_hash(dat$veg_plots_sdl_Species, "e4276e9361")
})

test_that("get_sdl_data formats data correctly", {
    skip_if_no_retriever()
    expect_error(dat <- get_sdl_data(), NA)
    expect_true(check_data_format(dat))
    expect_known_hash(dat, "e61d747927")
})
