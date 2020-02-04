#' @title Create Sonoran desert lab time-series data
#'
#' @description Original data found here http://www.eebweb.arizona.edu/faculty/venable/LTREB/LTREB%20data.htm
#'
#' @param plots vector of plots to keep
#' @inheritParams get_mtquad_data
#'
#' @return list of two dataframes (one with abundance data, the other with covariate data) 
#'   and one list of metadata.
#'
#' @examples
#' \dontrun{
#'   get_sdl_data(sdl_data_tables=retriever_data()$'veg-plots-sdl')
#' }
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
    metadata <- list(timename = "year", effort = NULL)
    
    out <- list("abundance" = abundance, 
                "covariates" = covariates, 
                "metadata" = metadata) %>%
        append_retriever_citation(path)
    
    return(out)
}
