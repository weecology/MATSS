#' @title get Shortgrass Steppe rodent data
#' 
#' @inheritParams get_mtquad_data
#' @return list of abundance, covariates, and metadata
#'
#' @export
get_sgs_data <- function(path = file.path(get_default_data_path(), 
                                         "shortgrass-steppe-lter"))
{
    data_tables <- import_retriever_data(path = path)
    sgs_data <- data_tables$shortgrass_steppe_lter_shortgrass_data
    
    # select key columns 
    # filter out unknown species and recaptures
    sgs_data <- sgs_data %>% 
        dplyr::select_at(c("session", "year", "veg", "web", "spp")) %>% 
        dplyr::filter(.data$spp != "NA", 
                      .data$spp != "")
    
    # get data into wide format
    # summarize counts for each species in each period
    sgs_abundance <- sgs_data %>%
        dplyr::count(.data$year, .data$session, .data$spp) %>%
        tidyr::spread(key = .data$spp, value = .data$n, fill = 0)
    
    season <- rep(0, nrow(sgs_abundance))
    season[grepl("Sep", sgs_abundance$session)] <- 0.5
    sgs_abundance$samples <- sgs_abundance$year + season
    
    # split into two dataframes and save
    abundance <- dplyr::select_at(sgs_abundance, dplyr::vars(-c("year", "session", "samples")))
    covariates <- dplyr::select_at(sgs_abundance, c("year", "session", "samples"))
    metadata <- list(timename = "samples", effort = NULL, period = 0.5)
    
    out <- list("abundance" = abundance, 
                "covariates" = covariates, 
                "metadata" = metadata) %>%
        append_retriever_citation(path)
    
    return(out)
}
