#' @title Read in the cowley lizard community data from a txt file
#'
#' Import cowley lizard data from data files
#' 
#' @return list of two dataframes (one with abundance data, the other with 
#'   covariate data) and one list of metadata.
#'
#' @export
#'
get_cowley_lizards <- function()
{
    get_cowley_data("cowleylizards.txt")
}

#' @title Read in the cowley snake community data from a txt file
#'
#' Import cowley snake data from data files
#' 
#' @return list of two dataframes (one with abundance data, the other with 
#'   covariate data) and one list of metadata.
#'
#' @export
#'
get_cowley_snakes <- function()
{
    get_cowley_data("cowleysnakes.txt")
}

#' @noRd
get_cowley_data <- function(file = "cowleylizards.txt")
{
    path <- system.file("extdata", file,
                        package = "MATSS", mustWork = TRUE)
    
    raw_data <- read.delim(path) %>%
        dplyr::mutate_if(is.numeric, list(~dplyr::na_if(., -99))) %>%
        dplyr::filter(!is.na(dplyr::select(., 4)))
    
    abundance <- dplyr::select(raw_data, -dplyr::one_of(c("Year", "Site", "Total")))
    
    covariates <- tibble::tibble(Year = c(1989, 1990, 1992, 1993, 1994, 1995, 1996, 
                                         1997, 1998, 1999, 2000, 2001, 2002, 2003), 
                                collection_date = as.Date(c("1989-04-22", "1990-04-28", 
                                                            "1992-04-18", "1993-04-24", 
                                                            "1994-04-24", "1995-04-29", 
                                                            "1996-04-20", "1997-04-26", 
                                                            "1998-04-25", "1999-04-24", 
                                                            "2000-04-29", "2001-04-21", 
                                                            "2002-04-27", "2003-04-26")), 
                                burn_status = factor(c("no", "yes", "no", "yes", 
                                                       "no", "yes", "no", "yes", 
                                                       "yes", "yes", "yes", "yes", 
                                                       "no", "yes")), 
                                air_temp = c(27, 17, 20, 19, 24, 15, 20, 
                                             16, 31, 12, 22, 22, 17, 29), 
                                cloud_cover = factor(c("no", "no", "yes", "yes", 
                                                       "no", "yes", "no", "yes", 
                                                       "no", "yes", "yes", "yes", 
                                                       "yes", "no")))
    
    metadata <- list(timename = "Year", effort = NULL, site = "CowleyCounty",
                     period = 1, 
                     is_community = TRUE, 
                     species_table = data.frame(id = colnames(abundance), 
                                                stringsAsFactors = FALSE) %>%
                         dplyr::mutate(species_name = gsub("_", " ", .data$id)), 
                     location = c("latitude" = 37.25, "longitude" = -(96+43/60)), 
                     citation = paste("Wilgers, DJ, Horne, EA, Sandercock, BK, Volkmann, AW, 2006.", 
                                      "EFFECTS OF RANGELAND MANAGEMENT ON COMMUNITY DYNAMICS OF THE",  
                                      "HERPETOFAUNA OF THE TALLGRASS PRAIRIE, Herpetologica, 62(4)."),
                     source_url = paste0("https://bioone.org/journals/Herpetologica/volume-62/",
                                         "issue-4/0018-0831(2006)62[378:EORMOC]2.0.CO;2/EFFECTS-OF-RANGELAND-",
                                         "MANAGEMENT-ON-COMMUNITY-DYNAMICS-OF-THE-HERPETOFAUNA/10.1655/0018-",
                                         "0831(2006)62[378:EORMOC]2.0.CO;2.full"))
    
    out <- list(abundance = abundance, 
                covariates = covariates,
                metadata = metadata)
    attr(out, "class") <- "matssdata"
    
    return(out)
}
