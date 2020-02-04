library(testthat)
library(MATSS)

if ("sample.kind" %in% names(formals(RNGkind)))
{
    suppressWarnings(RNGkind(sample.kind = "Rounding"))
}

# check if `retriever` is installed
skip_if_no_retriever <- function()
{
    have_retriever <- reticulate::py_module_available("retriever")
    if (!have_retriever)
        skip("retriever not available for testing")
}

expect_plan <- function(plan)
{
    eval(bquote(expect_true(tibble::is_tibble(.(plan)))))
    eval(bquote(expect_true("drake_plan" %in% class(.(plan)))))
    eval(bquote(expect_true(all(c("target", "command") %in% names(.(plan))))))
    eval(bquote(expect_equal(class(.(plan)$target), "character")))
    eval(bquote(expect_equal(class(.(plan)$command), "list")))
}

test_check("MATSS")
