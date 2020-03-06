context("Check Retriever datasets")

test_that("veg-plots-sdl data retrieval works correctly", {
    skip_if_no_retriever()
    test_path <- tempdir()
    Sys.setenv(MATSS_DATA_PATH = test_path)
    
    expect_error(install_retriever_data("veg-plots-sdl"), NA)
    expect_error(dat <- import_retriever_data("veg-plots-sdl"), NA)
    expect_false(is.null(dat))
    
    expect_error(dat <- get_sdl_data(), NA)
    species_table_content <- unlist(dat$metadata$species_table)
    expect_known_hash(species_table_content, "d98472ffcc")
    expect_known_hash(is.na(species_table_content), "4cada1727a")
    species_table_content[is.na(species_table_content)] <- "-999999"
    expect_identical(species_table_content[204], c("species_name29" = "Aplopappus hartwegii"))
    expect_known_hash(species_table_content, "54cd8373ad")
    
    expect_known_hash(dat$metadata$species_table, "5712adf07b") #
    expect_known_hash(is.na(dat$metadata$species_table), "a3ea041385")
    dat$metadata$species_table[is.na(dat$metadata$species_table)] <- "-999999"
    expect_known_hash(dat$metadata$species_table, "d750b72bc9") # 
    # expect_known_hash(dat$metadata$species_table$reportedname, "2688f9de54")
    # expect_known_hash(dat$metadata$species_table$species_name, "d679dd9ce8")
    expect_dataset(dat, "b076948e1b", "de2dc7f655", "d52a190865")
})
