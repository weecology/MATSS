#' @title get Jornada rodent data
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
    jornada_rodents <- jornada %>%
        dplyr::select_at(c("year", "season", "spp", "recap")) %>% 
        dplyr::filter(.data$recap != "Y", 
                      !.data$spp %in% c("DIPO1", "PERO1", "NA", "."), 
                      !is.na(.data$spp))
    
    # get data into wide format
    # summarize counts for each species in each period
    jornada_abundances <- jornada_rodents %>%
        dplyr::count(.data$year, .data$season, .data$spp) %>%
        tidyr::spread(key = .data$spp, value = .data$n, fill = 0)
    
    season <- rep(0, nrow(jornada_abundances))
    season[which(jornada_abundances$season == "F")] <- 0.5
    jornada_abundances$time <- jornada_abundances$year + season
    
    # split into two dataframes and save
    covariates <- dplyr::select_at(jornada_abundances, c("year", "season", "time"))
    abundance <- dplyr::select_at(jornada_abundances, dplyr::vars(-c("year", "season", "time")))
    species_table = tibble::tibble(id = c("CHPE", 
                                          "DIME", 
                                          "DIOR", 
                                          "DISP", 
                                          "MUMU", 
                                          "NEAL", 
                                          "NEMI", 
                                          "ONAR", 
                                          "ONLE", 
                                          "PEBO", 
                                          "PEER", 
                                          "PELE", 
                                          "PEMA", 
                                          "PGFL",
                                          "REME", 
                                          "SIHI", 
                                          "SPSP"),
                                   genus = c("Chaetodipus", 
                                             "Dipodomys", 
                                             "Dipodomys", 
                                             "Dipodomys", 
                                             "Mus", 
                                             "Neotoma", 
                                             "Neotoma", 
                                             "Onychomys",
                                             "Onychomys", 
                                             "Peromyscus", 
                                             "Peromyscus", 
                                             "Peromyscus", 
                                             "Peromyscus", 
                                             "Perognathus", 
                                             "Reithrodontomys", 
                                             "Sigmodon", 
                                             "Spermophilus"), 
                                   species = c("penicillatus", 
                                               "merriami", 
                                               "ordii", 
                                               "spectabilis", 
                                               "musculus", 
                                               "albigula", 
                                               "micropus", 
                                               "arenicola", 
                                               "leucogaster", 
                                               "boylii", 
                                               "eremicus", 
                                               "leucopus", 
                                               "maniculatus", 
                                               "flavus", 
                                               "megalotis", 
                                               "hispidus", 
                                               "spilosoma"))
    metadata <- list(timename = "time", period = 0.5, effort = NULL, 
                     species_table = species_table, 
                     is_community = TRUE, 
                     location = c("latitude" = 32.6, 
                                  "longitude" = -106.7))
    
    out <- list(abundance = abundance, 
                covariates = covariates, 
                metadata = metadata) %>%
        append_retriever_citation(path)
    attr(out, "class") <- "matssdata"
    
    return(out)
}
