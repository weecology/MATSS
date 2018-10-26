#' @importFrom magrittr "%>%"

process_jornada_data <- function(data_path = "data/jornada_rodents.csv"){
    
    # read in Jornada rodent data
    jornada <- read.csv(data_path)
    
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
    covariates <- jornada_abundance_table[,1:2]
    abundance <- jornada_abundance_table[,-c(1:2)]
    
    jornada_raw <- list(abundance, covariates)
    jornada_raw <- setNames(jornada_raw, c("abundance", "covariates"))
    return(jornada_raw)
    
}

