context("Check Retriever datasets")

test_that("veg-plots-sdl data retrieval works correctly", {
    skip_if_no_retriever()
    test_path <- tempdir()
    Sys.setenv(MATSS_DATA_PATH = test_path)
    
    expect_error(install_retriever_data("veg-plots-sdl"), NA)
    expect_error(dat <- import_retriever_data("veg-plots-sdl"), NA)
    expect_false(is.null(dat))
    
    expect_error(dat <- get_sdl_data(), NA)
    expect_known_hash(dat$metadata$species_table, "1ceab10626")
    expect_known_hash(dat$metadata$species_table$reportedname, "2688f9de54")
    expect_known_hash(dat$metadata$species_table$species_name, "42c0957e37")
    MATSS:::expect_dataset(dat, "b076948e1b", "de2dc7f655", "96918f0dc5")
})
