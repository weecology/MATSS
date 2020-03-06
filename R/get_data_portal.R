#' @title get portal rodent data
#' 
#' Import Portal rodent data using portalr functions. 
#' Currently returns rodent data formatted appropriately for
#' LDA analysis. 
#' 
#' @param time_or_plots select whether to: (1) "time" == get the data for the 
#'   entire timespan of the experiment, or (2) "plots" == just the time period 
#'   with consistent treatments
#' @param treatment "control" or "exclosure" treatments
#' @param type type of animals to get: "Rodents" or restrict to "Granivores"
#' 
#' @return list of two dataframes (one with abundance data, the other with covariate data) 
#'   and one list of metadata.
#'
#' @export
get_portal_rodents <- function(time_or_plots = "plots", 
                               treatment = "control",
                               type = "Rodents")
{
    # set params
    if (tolower(time_or_plots) == "plots") {
        plots <- "all"
        start_period <- 118
        standard_effort <- 8
    } else if (tolower(time_or_plots) == "time") {
        plots <- "longterm"
        start_period <- 1
        standard_effort <- 4
    }
    
    # get raw data by plot
    dat <- portalr::abundance(path = "repo", clean = FALSE, 
                              level = "plot", type = type, 
                              plots = plots, 
                              unknowns = FALSE, shape = 'flat',
                              time = "all", effort = TRUE, 
                              min_plots = 0)
    
    # filter according to treatment
    if (treatment == 'exclosure')
    {
        dat <- dplyr::filter(dat, .data$treatment == "exclosure")
    } else if (treatment == "control") {
        dat <- dplyr::filter(dat, .data$plot %in% c(2, 4, 8, 11, 12, 14, 17, 22))
    }
    
    # summarize by period, computing weighted abundance by effort
    dat2 <- dat %>%
        dplyr::filter(.data$period %in% start_period:436, 
                      .data$ntraps >= 1) %>%
        dplyr::select(-.data$period, -.data$ntraps) %>%
        dplyr::group_by(.data$censusdate, .data$species, .data$newmoonnumber) %>%
        dplyr::summarize(abundance = round(standard_effort * mean(.data$abundance, na.rm = TRUE) + 1e-10)) %>%
        dplyr::ungroup() %>%
        tidyr::spread(.data$species, .data$abundance)
    
    species_codes <- setdiff(names(dat2), c("censusdate", "newmoonnumber"))
    species_table <- portalr::load_datafile(file.path("Rodents", "Portal_rodent_species.csv"),
                                   na.strings = "", "repo") %>%
        dplyr::rename(id = .data$speciescode) %>%
        dplyr::filter(.data$id %in% species_codes) %>%
        tidyr::separate(.data$scientificname, c("genus", "species"), sep = " ")
    
    abundance <- dplyr::select(dat2, -.data$newmoonnumber, -.data$censusdate)
    covariates <- dplyr::select(dat2, .data$newmoonnumber, .data$censusdate)
    metadata <- list(timename = "newmoonnumber", effort = NULL, 
                     species_table = species_table, 
                     is_community = TRUE, 
                     location = c("latitude" = 31.938, 
                                  "longitude" = -109.08), 
                     citation = portalr::get_dataset_citation()$textVersion)
    
    out <- list(abundance = abundance, 
                covariates = covariates, 
                metadata = metadata)
    return(out)
}