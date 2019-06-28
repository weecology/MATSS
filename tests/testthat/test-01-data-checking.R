context("Validation Function for Data Format")

set.seed(42)

data(dragons)

test_that("check_data_format works on basic examples", {
    
    # basic type checking
    expect_false(check_data_format(Nile))
    expect_false(check_data_format(mtcars))
    expect_false(check_data_format(list(mtcars)))
    
    # abundance type checking
    expect_true(check_data_format(list(abundance = mtcars)))
    expect_false(check_data_format(list(abundance = iris)))
    expect_false(check_data_format(list(abundance = Nile)))
    
    # covariates format checking
    expect_true(check_data_format(list(abundance = mtcars, 
                                       covariates = mtcars)))
    expect_false(check_data_format(list(abundance = mtcars, 
                                        covariates = Nile)))
    expect_false(check_data_format(list(abundance = mtcars, 
                                        covariates = iris)))
    
    # working example
    expect_true(check_data_format(dragons))
})

test_that("get_effort_from_data works on basic examples", {
    expect_null(get_effort_from_data(Nile))
    expect_null(get_effort_from_data(mtcars))
    expect_null(get_effort_from_data(list(abundance = mtcars, 
                                          covariates = mtcars)))
    expect_equal(get_effort_from_data(dragons), 
                 c(3, 3, 2, 4, 1, 9))
})

test_that("get_times_from_data works on basic examples", {
    expect_null(get_times_from_data(Nile))
    expect_null(get_times_from_data(mtcars))
    expect_null(get_times_from_data(list(abundance = mtcars, 
                                         covariates = mtcars)))
    expect_equal(get_times_from_data(dragons), 
                 seq.Date(from = as.Date("2014-06-28"), by = "1 year", length.out = 6))
})
                 