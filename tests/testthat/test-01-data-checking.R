context("test-data-checking")

test_that("check_data_format works on basic examples", {
    expect_false(check_data_format(mtcars))
    expect_false(check_data_format(list(mtcars)))
    
    expect_true(check_data_format(list(abundance = mtcars)))
    
    expect_true(check_data_format(list(abundance = mtcars, 
                                       covariates = mtcars)))
    
    expect_false(check_data_format(list(abundance = mtcars, 
                                        covariates = iris)))
    
    expect_true(check_data_format(list(abundance = data.frame("Red Spotted Dragon" = c(2,6,0,5,4,4),
                                                              "Green Striped Dragon" = c(6,0,4,1,9,7),
                                                              "Blue Eyes White Dragon" = c(0,0,0,1,0,0)), 
                                       covariates = data.frame("date" = rev(seq.Date(from = Sys.Date(), by = "-1 year", length.out = 6)), 
                                                               "precipitation" = rpois(6, lambda = 10)))))
})
    
    