#' @importFrom magrittr "%>%"

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
                              time = "all", effort = TRUE)

    # filter according to treatment
    if (treatment == 'exclosure')
    {
        dat <- dplyr::filter(dat, treatment == "exclosure")
    } else if (treatment == "control") {
        dat <- dplyr::filter(dat, plot %in% c(2, 4, 8, 11, 12, 14, 17, 22))
    }

    # summarize by period, computing weighted abundance by effort
    dat2 <- dat %>%
        dplyr::filter(period %in% start_period:436,
                      ntraps >= 1) %>%
        dplyr::select(-period, -ntraps) %>%
        dplyr::group_by(censusdate, species, newmoonnumber) %>%
        dplyr::summarize(abundance = round(standard_effort * mean(abundance, na.rm = TRUE) + 1e-10)) %>%
        dplyr::ungroup() %>%
        tidyr::spread(species, abundance)


  list(abundance = dplyr::select(dat2, -newmoonnumber, -censusdate),
       covariates = dplyr::select(dat2, newmoonnumber, censusdate),
       metadata = list(times = "newmoonnumber", effort = NULL))

}
