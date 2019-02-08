#' @importFrom magrittr "%>%"

#' @title get Shortgrass Steppe rodent data
#' 
#' Import Shortgrass Steppe rodent abundance from data files
#' 
#' @param data_path  location of the raw data
#' 
#' @export
process_sgs_data <- function()
{
    
    # read in Shortgrass Steppe rodent data
    data_path <- system.file("extdata", "shortgrass_steppe_rodents.csv", 
                             package = "MATSS", mustWork = TRUE)
    sgs_data <- read.csv(data_path)
    
    # select key columns 
    # filter out unknown species and recaptures
    sgs_data <- dplyr::select(sgs_data, SESSION, YEAR, VEG, WEB, SPP) %>% 
        dplyr::filter(SPP != 'NA')
    
    # get data into wide format
    # summarize counts for each species in each period
    sgs_abundances <- sgs_data %>%
        dplyr::group_by(YEAR, SESSION, SPP) %>%
        dplyr::summarize(count = n())
    
    # put data in wide format
    sgs_abundance_table <- sgs_abundances %>%
        tidyr::spread(SPP, count, fill = 0)
    
    # split into two dataframes and save
    covariates <- sgs_abundance_table[,1:2]
    abundance <- sgs_abundance_table[,-c(1:2)]
    
    return(mget(c("abundance", "covariates")))
    
}