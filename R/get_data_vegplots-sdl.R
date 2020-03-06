#' @title Create Sonoran desert lab time-series data
#'
#' @description Original data found here http://www.eebweb.arizona.edu/faculty/venable/LTREB/LTREB%20data.htm
#'
#' @param plots vector of plots to keep
#' @inheritParams get_mtquad_data
#' @return list of abundance, covariates, and metadata
#' 
#' @export
get_sdl_data <- function(plots = c(4, 7, 8, 9, 10, 11, 12, 14, 15, 16, 17), 
                         path = file.path(get_default_data_path(), "veg-plots-sdl"))
{
    sdl_data_tables <- import_retriever_data(path = path)
    
    sdl_data <- sdl_data_tables$veg_plots_sdl_SMDensity %>%
        dplyr::select(-.data$countns) %>%
        dplyr::filter(.data$plot %in% plots) %>%
        dplyr::group_by(.data$year, .data$code) %>%
        dplyr::summarize(count = sum(.data$count)) %>%
        tidyr::spread(key = .data$code, value = .data$count, fill = 0) %>%
        dplyr::rename(UNKN = .data$V1) %>%
        dplyr::ungroup()
    
    abundance <- dplyr::select(sdl_data, -.data$year)
    covariates <- dplyr::select(sdl_data, .data$year)
    species_table <- sdl_data_tables$veg_plots_sdl_Species %>%
        dplyr::rename(id = .data$code, 
                      species_name = .data$acceptedname)
    Encoding(species_table$reportedname) <- "latin1"
    Encoding(species_table$species_name) <- "latin1"
    species_table[, c("species_name", "var_subsp")] <- 
        stringr::str_split(species_table$species_name, "\\ssubsp\\.\\s|\\svar\\.\\s", simplify = TRUE)
    species_table[, c("genus", "species")] <-
        stringr::str_split(species_table$species_name, "\\s", simplify = TRUE)
    species_table <- species_table %>%
        dplyr::mutate(species = ifelse(nchar(.data$species) == 0, NA, .data$species), 
                      var_subsp = ifelse(nchar(.data$var_subsp) == 0, NA, .data$var_subsp)) %>%
        dplyr::select(.data$id, .data$species_name, 
                      .data$family, .data$genus, .data$species, .data$var_subsp, 
                      dplyr::everything()) %>%
        tibble::add_row(id = "UNKN") %>%
        as.data.frame()
    for (j in seq(NCOL(species_table)))
    {
        Encoding(species_table[, j]) <- "unknown"
    }
    
    metadata <- list(timename = "year", effort = NULL,
                     species_table = species_table, 
                     location = c("latitude" = 32.21, 
                                  "longitude" = -111.01), 
                     is_community = TRUE)
    
    out <- list("abundance" = abundance, 
                "covariates" = covariates, 
                "metadata" = metadata) %>%
        append_retriever_citation(path)
    
    return(out)
}
