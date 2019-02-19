#' @title Read in the maizuru community data from a csv file
#'
#' Import maizuru data from data files
#' 
#' @return list of two dataframes (one with abundance data, the other with 
#'   covariate data) and one list of metadata.
#'
#' @export
#'
get_maizuru_data <- function()
{
    data_path <- system.file("extdata", "Maizuru_dominant_sp.csv",
                             package = "MATSS", mustWork = TRUE)
    raw_data <- read.csv(data_path)
    raw_data$Date <- dplyr::select(raw_data, Y, M, D) %>%
        apply(1, paste, collapse = "-") %>%
        as.Date()
    
    list(abundance = dplyr::select(raw_data, -date_tag, -surf.t, -bot.t, -Y, -M,
                                   -D, -Date) %>%
             dplyr::mutate_all(~round(. + 1e-10)), 
         covariates = dplyr::select(raw_data, date_tag, surf.t, bot.t, Y, M, D, 
                                    Date),
         metadata = list(timename = "Date", effort = NULL))
}

