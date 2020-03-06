# check if `retriever` is installed
#' @noRd
skip_if_no_retriever <- function()
{
    have_retriever <- reticulate::py_module_available("retriever")
    if (!have_retriever)
        testthat::skip("retriever not available for testing")
}

#' @noRd
expect_plan <- function(plan)
{
    eval(bquote(expect_true(tibble::is_tibble(.(plan)))))
    eval(bquote(expect_true("drake_plan" %in% class(.(plan)))))
    eval(bquote(expect_true(all(c("target", "command") %in% names(.(plan))))))
    eval(bquote(expect_equal(class(.(plan)$target), "character")))
    eval(bquote(expect_equal(class(.(plan)$command), "list")))
}

#' @noRd
expect_dataset <- function(dat, 
                           abundance_hash = "", 
                           covariates_hash = "", 
                           metadata_hash = "")
{
    eval(bquote(testthat::expect_true(check_data_format(.(dat)))))
    dat$metadata$citation <- NULL
    eval(bquote(testthat::expect_known_hash(.(dat)$abundance, .(abundance_hash))))
    eval(bquote(testthat::expect_known_hash(.(dat)$covariates, .(covariates_hash))))
    eval(bquote(testthat::expect_known_hash(.(dat)$metadata, .(metadata_hash))))
}
