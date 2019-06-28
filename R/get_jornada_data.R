#' @title get Jornada rodent data
#' 
#' Import Jornada rodent abundance from data files
#' 
#' @return list of two dataframes (one with abundance data, the other with
#'   covariate data), and one list of metadata.
#'
#' @export
get_jornada_data <- function()
{
    # read in Jornada rodent data
    data_path <- system.file("extdata", "jornada_rodents.csv", 
                             package = "MATSS", mustWork = TRUE)
    jornada <- utils::read.csv(data_path)
    
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
    jornada_raw <- list(abundance, covariates, metadata)
    jornada_raw <- stats::setNames(jornada_raw, c("abundance", "covariates", "metadata"))
    return(jornada_raw)
}

