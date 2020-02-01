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
    
    return(list('abundance' = abundance, 'covariates' = covariates, 
                "metadata" = metadata))
}

#' @title Create Montana plant quad time-series data
#'
#' @description Original data found here 
#'
#' @param path where to load the raw data files from
#'
#' @return list of two dataframes (one with abundance data, the other with covariate data) 
#'   and one list of metadata.
#' 
#' @examples
#' \dontrun{
#'   get_mtquad_data(mtquad_data_tables=retriever_data()$'mapped-plant-quads-mt')
#' }
#' @export

get_mtquad_data <- function(path = file.path(get_default_data_path(), "mapped-plant-quads-mt"))
{
    mtquad_data_tables <- import_retriever_data(path = path)
    
    mtquad_data <- mtquad_data_tables$mapped_plant_quads_mt_allrecords_density %>%
        dplyr::select(-.data$objectid, -.data$seedling, -.data$x, -.data$y) %>%
        dplyr::group_by(.data$year, .data$species, .data$quad) %>%
        dplyr::summarize(abundance = sum(.data$stems)) %>%
        dplyr::group_by(.data$year, .data$species) %>%
        dplyr::summarize(abundance = sum(.data$abundance)) %>%
        tidyr::spread(key = .data$species, value = .data$abundance, fill = 0) %>%
        dplyr::ungroup()
    
    abundance <- dplyr::select(mtquad_data, -.data$year)
    
    covariates <- dplyr::select(mtquad_data, .data$year)
    
    metadata <- list(timename = "year", effort = NULL)
    return(list('abundance' = abundance, 'covariates' = covariates, 
                "metadata" = metadata))
}

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
get_maizuru_data <- function(path = file.path(get_default_data_path(), "ushio-maizuru-fish-community"))
{
    data_tables <- import_retriever_data(path = path)
    
    raw_data <- data_tables$ushio_maizuru_fish_community_maizuru
    raw_data$date <- dplyr::select(raw_data, .data$y, .data$m, .data$d) %>%
        apply(1, paste, collapse = "-") %>%
        as.Date()
    
    covars <- c("date_tag", "surf_t", "bot_t", "y", "m", "d", "date")
    list(abundance = raw_data %>% 
             dplyr::select(-dplyr::one_of(covars)) %>%
             dplyr::mutate_all(~round(. + 1e-10)), 
         covariates = raw_data %>% 
             dplyr::select_at(covars),
         metadata = list(timename = "Date", effort = NULL))
}

