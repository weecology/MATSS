#' @title get Jornada rodent data
#' 
#' Import Jornada rodent abundance from data files
#' 
#' 
#' @inheritParams get_mtquad_data
#' @return list of abundance, covariates, and metadata
#' 
#' @export
get_jornada_data <- function(path = file.path(get_default_data_path(), 
                                              "jornada-lter-rodent"))
{
    # read in Jornada rodent data
    data_tables <- import_retriever_data(path = path)
    jornada <- data_tables$jornada_lter_rodent_smes_rodent_trapping

    # select key columns 
    # filter out unknown species and recaptures
    jornada_rodents <- dplyr::select(jornada, .data$year, .data$season, .data$spp, .data$recap) %>% 
        dplyr::filter(.data$recap != "Y", !.data$spp %in% c("DIPO1", "PERO1", "NA", "."))
    
    # get data into wide format
    # summarize counts for each species in each period
    jornada_abundances <- jornada_rodents %>%
        dplyr::group_by(.data$year, .data$season, .data$spp) %>%
        dplyr::summarize(count = dplyr::n())
    
    # put data in wide format
    jornada_abundance_table <- jornada_abundances %>%
        tidyr::spread(.data$spp, .data$count, fill = 0)
    
    season <- rep(0, nrow(jornada_abundance_table))
    season[which(jornada_abundance_table$season == "F")] <- 0.5
    jornada_abundance_table$time <- jornada_abundance_table$year + season
    
    # split into two dataframes and save
    covariates <- jornada_abundance_table[, c("year", "season", "time")]
    abundance <- jornada_abundance_table[, -which(colnames(jornada_abundance_table) %in% c("year", "season", "samples", "time"))]
    metadata <- list(timename = "time", period = 0.5, effort = NULL)
    
    out <- list(abundance = abundance, 
                covariates = covariates, 
                metadata = metadata) %>%
        append_retriever_citation(path)
    
    return(out)
}
