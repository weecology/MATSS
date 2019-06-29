#' Dataset Maizuru Fish Community
#' 
#' @name maizuru_data
#' @author Reiji Masuda
#' @description Time series of twice-monthly visual census data of the Maizuru 
#'   fish community (only the 15 species that had total observation count > 
#'   1000). As used in "Fluctuating interaction network and time-varying 
#'   stability of a natural fish community" (Ushio et al. 2018)
#' Data are available from https://zenodo.org/record/1181937
#' 
#' @format
#' \describe{
#'   \item{\code{date_tag}}{date of sampling}
#'   \item{\code{surf.t}}{surface temperature (in Celsius)}
#'   \item{\code{bot.t}}{bottom temperature (in Celsius)}
#'   \item{\code{Aurelia.sp}}{survey data on Jellyfish}
#'   \item{\code{Engraulis.japonicus}}{survey data on \emph{Engraulis japonicus}}
#'   \item{\code{Plotosus.lineatus}}{survey data on \emph{Plotosus lineatus}}
#'   \item{\code{Sebastes.inermis}}{survey data on \emph{Sebastes inermis}}
#'   \item{\code{Trachurus.japonicus}}{survey data on \emph{Trachurus japonicus}}
#'   \item{\code{Girella.punctata}}{survey data on \emph{Girella punctata}}
#'   \item{\code{Pseudolabrus.sieboldi}}{survey data on \emph{Pseudolabrus sieboldi}}
#'   \item{\code{Halichoeres.poecilopterus}}{survey data on \emph{Halichoeres poecilopterus}}
#'   \item{\code{Halichoeres.tenuispinnis}}{survey data on \emph{Halichoeres tenuispinnis}}
#'   \item{\code{Chaenogobius.gulosus}}{survey data on \emph{Chaenogobius gulosus}}
#'   \item{\code{Pterogobius.zonoleucus}}{survey data on \emph{Pterogobius zonoleucus}}
#'   \item{\code{Tridentiger.trigonocephalus}}{survey data on \emph{Tridentiger trigonocephalus}}
#'   \item{\code{Siganus.fuscescens}}{survey data on \emph{Siganus fuscescens}}
#'   \item{\code{Sphyraena.pinguis}}{survey data on \emph{Sphyraena pinguis}}
#'   \item{\code{Rudarius.ercodes}}{survey data on \emph{Rudarius ercodes}}
#'   \item{\code{Y}}{year}
#'   \item{\code{M}}{month}
#'   \item{\code{D}}{day}
#' }
#' 
#' @section Maizuru_dominant_sp.csv:
#' This data is imported using \code{\link{get_maizuru_data}}
NULL

#' @title Read in the maizuru community data from a csv file
#'
#' Import maizuru data from data files
#' 
#' @return list of two dataframes (one with abundance data, the other with 
#'   covariate data) and one list of metadata.
#'
#' @export
get_maizuru_data <- function()
{
    data_path <- system.file("extdata", "Maizuru_dominant_sp.csv",
                             package = "MATSS", mustWork = TRUE)
    raw_data <- utils::read.csv(data_path)
    raw_data$Date <- dplyr::select(raw_data, .data$Y, .data$M, .data$D) %>%
        apply(1, paste, collapse = "-") %>%
        as.Date()
    
    covars <- c("date_tag", "surf.t", "bot.t", "Y", "M", "D", "Date")
    list(abundance = raw_data %>% 
             dplyr::select(-dplyr::one_of(covars)) %>%
             dplyr::mutate_all(~round(. + 1e-10)), 
         covariates = raw_data %>% 
             dplyr::select(dplyr::one_of(covars)),
         metadata = list(timename = "Date", effort = NULL))
}

