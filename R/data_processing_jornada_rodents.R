#' @importFrom magrittr "%>%"

process_jornada_data <- function(data_path = "data/", 
                                 rodent_file = file.path(data_path, "jornada_rodents.csv"), 
                                 abundance_file = file.path(data_path, "jornada_abundance_table.RDS"),
                                 covariates_file = file.path(data_path, "jornada_covariates_time.RDS"))
{
    # read in Jornada rodent data
    jornada <- read.csv(rodent_file)
    
    # select key columns 
    # filter out unknown species and recaptures
    jornada_rodents <- dplyr::select(jornada, year, season, spp, recap) %>% 
        dplyr::filter(recap != "Y", spp != "DIPO1", spp != "PERO1", spp != "NA", spp != ".")
    
    # get data into wide format
    # summarize counts for each species in each period
    jornada_abundances <- jornada_rodents %>%
        dplyr::group_by(year, season, spp) %>%
        dplyr::summarize(count = n())
    
    # put data in wide format
    jornada_abundance_table <- jornada_abundances %>%
        tidyr::spread(spp, count, fill = 0)
    
    # split into two dataframes and save
    jornada_covariates_time <- jornada_abundance_table[,1:2]
    jornada_abundance_table <- jornada_abundance_table[,-c(1:2)]
    
    saveRDS(jornada_abundance_table, file = abundance_file)
    saveRDS(jornada_covariates_time, file = covariates_file)
    return()
       
}

