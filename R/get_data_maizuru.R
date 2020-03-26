#' @title get the maizuru community data
#'
#' @inheritParams get_mtquad_data
#' @return list of abundance, covariates, and metadata
#' 
#' @export
get_maizuru_data <- function(path = file.path(get_default_data_path(), 
                                              "ushio-maizuru-fish-community"))
{
    data_tables <- import_retriever_data(path = path)
    
    raw_data <- data_tables$ushio_maizuru_fish_community_maizuru
    raw_data$date <- dplyr::select(raw_data, .data$y, .data$m, .data$d) %>%
        apply(1, paste, collapse = "-") %>%
        as.Date()
    
    species_table <- tibble::tibble(id = c("aurelia_sp", 
                                           "engraulis_japonicus", 
                                           "plotosus_japonicus", 
                                           "sebastes_inermis", 
                                           "trachurus_japonicus", 
                                           "girella_punctata", 
                                           "pseudolabrus_sieboldi", 
                                           "parajulis_poecilepterus", 
                                           "halichoeres_tenuispinis", 
                                           "chaenogobius_gulosus", 
                                           "pterogobius_zonoleucus", 
                                           "tridentiger_trigonocephalus", 
                                           "siganus_fuscescens", 
                                           "sphyraena_pinguis", 
                                           "rudarius_ercodes"),
                                    genus = c("Aurelia", 
                                              "Engraulis", 
                                              "Plotosus", 
                                              "Sebastes", 
                                              "Trachurus", 
                                              "Girella", 
                                              "Pseudolabrus", 
                                              "Parajulis",
                                              "Halichoeres", 
                                              "Chaenogobius", 
                                              "Pterogobius", 
                                              "Tridentiger", 
                                              "Siganus", 
                                              "Sphyraena", 
                                              "Rudarius"), 
                                    species = c(NA, 
                                                "japonicus", 
                                                "japonicus", 
                                                "inermis", 
                                                "japonicus", 
                                                "punctata", 
                                                "sieboldi", 
                                                "poecilepterus", 
                                                "tenuispinis", 
                                                "gulosus", 
                                                "zonoleucus", 
                                                "trigonocephalus", 
                                                "fuscescens", 
                                                "pinguis", 
                                                "ercodes"))
    
    covars <- c("date_tag", "surf_t", "bot_t", "y", "m", "d", "date")
    out <- list(abundance = raw_data %>% 
                    dplyr::select(-dplyr::one_of(covars)) %>%
                    dplyr::mutate_all(~round(. + 1e-10)), 
                covariates = raw_data %>% 
                    dplyr::select_at(covars),
                metadata = list(timename = "date", effort = NULL, 
                                species_table = species_table, 
                                location = c("latitude" = 35 + 28/60, 
                                             "longitude" = 135 + 22/60), 
                                is_community = TRUE)) %>%
        append_retriever_citation(path)
    attr(out, "class") <- "matssdata"
    
    return(out)
}
