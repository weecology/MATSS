context("MATSS research compendium")

test_path <- file.path(tempdir(), "test")

test_that("creating a compendium works", {
    expect_error(create_MATSS_compendium(test_path), NA)
})

test_that("compendium files exist", {
    expect_true(file.exists(test_path))
    expect_true(file.exists(file.path(test_path, "DESCRIPTION")))
    expect_true(file.exists(file.path(test_path, "NAMESPACE")))
    expect_true(file.exists(file.path(test_path, ".gitignore")))
    expect_true(file.exists(file.path(test_path, ".Rbuildignore")))
    expect_equal(file.exists(file.path(test_path, "test.Rproj")), 
                 rstudioapi::isAvailable())
    expect_true(file.exists(file.path(test_path, "analysis")))
    expect_true(file.exists(file.path(test_path, "analysis", "pipeline.R")))
    expect_true(file.exists(file.path(test_path, "analysis", "report.Rmd")))
    expect_true(file.exists(file.path(test_path, "R")))
})
