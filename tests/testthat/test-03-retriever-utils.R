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
    expect_error(install_retriever_data("iris"), NA)
    m <- capture_messages(install_retriever_data("iris"))
    expect_match(m, "A folder already exists for \"iris\"")
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
    expect_error(dat <- import_retriever_data("iris"), NA)
    expect_known_hash(dat$iris_Iris, "e8f8676f3c")
})
