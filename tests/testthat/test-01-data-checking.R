context("test data-checking function")

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
    expect_true(check_data_format(list(abundance = data.frame("Red Spotted Dragon" = c(2,6,0,5,4,4),
                                                              "Green Striped Dragon" = c(6,0,4,1,9,7),
                                                              "Blue Eyes White Dragon" = c(0,0,0,1,0,0)), 
                                       covariates = data.frame("date" = rev(seq.Date(from = Sys.Date(), by = "-1 year", length.out = 6)), 
                                                               "precipitation" = rpois(6, lambda = 10)))))
})