# Downloading data from `popler` 
devtools::install_github("AldoCompagnoni/popler", build_vignettes = TRUE)
library(popler)
library(testit)

# find datasets by subsetting the metadata table
community_metadata <- pplr_browse(duration_years >= 10 & community == 'yes')

# download the data using the get_data() function
community_datasets <- pplr_get_data(community_metadata)
community_datasets_cov <- pplr_cov_unpack(community_metadata)

# pull number column
# for loop for downloading w/ a dataframe to record any errors
# skip error function (metacom practice)

proj_keys <- community_metadata$proj_metadata_key
proj_keys_test <- c(2,3,4,12,13)

error_df <- data.frame(project_key = integer(length(proj_keys)),
                       error = character(length(proj_keys)))
list_df <- list()
list_num = 1

for (i in proj_keys_test) {
  
  n <- list_num
  i = i
  
  if (has_error(pplr_get_data(proj_metadata_key == i))){
    error_df$project_key[n] <- i
    error_df$error[n] <- "error"
  } else {
    error_df$project_key[n] <- i
    error_df$error[n] <- NA
    list_df[[n]] <- pplr_get_data(proj_metadata_key == as.integer(i))
  }
  
  list_num <- n + 1
  
}


###############################################################
data <- pplr_get_data(proj_metadata_key == 2)

