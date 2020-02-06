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
    
    covars <- c("date_tag", "surf_t", "bot_t", "y", "m", "d", "date")
    out <- list(abundance = raw_data %>% 
                    dplyr::select(-dplyr::one_of(covars)) %>%
                    dplyr::mutate_all(~round(. + 1e-10)), 
                covariates = raw_data %>% 
                    dplyr::select_at(covars),
                metadata = list(timename = "Date", effort = NULL)) %>%
        append_retriever_citation(path)
    
    return(out)
}
