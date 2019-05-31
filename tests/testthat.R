library(testthat)
library(MATSS)

if ("sample.kind" %in% names(formals(RNGkind)))
{
    suppressWarnings(RNGkind(sample.kind = "Rounding"))
}

# check if `retriever` is installed
skip_if_no_retriever <- function() {
    have_retriever <- reticulate::py_module_available("retriever")
    if (!have_retriever)
        skip("retriever not available for testing")
}

test_check("MATSS")
