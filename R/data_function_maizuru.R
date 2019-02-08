#' @title Read in the maizuru community data from a csv file
#'
#' Import maizuru data from data files
#' 
#' @return list of two dataframes (one with abundance data, the other with covariate data) 
#'   and one list of metadata.
#'
#' @export
#'
get_maizuru_data <- function()
{
  x <- read.csv(here::here("data", "Maizuru_dominant_sp.csv"))
  x$Date <- dplyr::select(x, Y, M, D) %>%
            apply(1, paste, collapse = "-") %>%
            as.Date()
  list(abundance = dplyr::select(x, -date_tag, -surf.t, -bot.t, -Y, -M, -D, -Date) %>%
                            mutate_all(~round(. + 1e-10)), 
                        covariates = dplyr::select(x, date_tag, surf.t, bot.t, Y, M, D, Date),
                        metadata = list(timename = "Date", effort = NULL))
}

