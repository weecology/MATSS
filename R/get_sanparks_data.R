#' @title get Karoo ungulate data
#' 
#' Import and clean Karoo abundance from data files
#' 
#' @return list of two dataframes (one with abundance data, the other with
#'   covariate data), and one list of metadata.
#'
#' @importFrom utils read.delim
#' @export

get_karoo_data <- function()
{
    species_path <- system.file("extdata", "sanparks/peggym.114.1-KarooNationalParkCensuscodes.txt",
                                package = "MATSS", mustWork = TRUE)
    #Better organize species table    
    species <- read.delim(species_path) %>%
        dplyr::mutate(target = c(0,1,rep(0,7),rep(1,2),0,1,0,1,0,0,1,0,1,0,1,1,0,0,rep(1,4)), 
                      Common.name = gsub("\\s+","",as.character(.data$Common.name)), 
                      Species = tolower(as.character(.data$Species))) %>%
        rbind(c("BlackRhino","br",1), c("Burchellszebra","bz",1),c("Blesbok","bl",1))
    
    process_karoo <- function(df, level_key = c("RedHartebeest" = "Redhartebeest", 
                                                "Mtnzebra" = "Mountainzebra", 
                                                "MtReedbuck" = "Mountainreedbuck"))
    {
        df %>%
            dplyr::mutate(Date = as.Date(.data$Date)) %>%
            dplyr::mutate(Common.name = gsub("\\s+", "", .data$Common.name)) %>%
            dplyr::mutate(Common.name = dplyr::recode(.data$Common.name, !!!level_key)) %>%
            dplyr::left_join(species, by = c("Common.name" = "Common.name")) %>%
            dplyr::select(c("Date","Common.name","total","target","Latitude","Longitude"))
    }
    
    # Data tables are all formatted slightly differently
    karoo1 <- system.file("extdata", "sanparks/peggym.116.1-KRNPTotals.txt",
                          package = "MATSS", mustWork = TRUE) %>%
        read.delim() %>% 
        dplyr::mutate(Latitude = NA, Longitude = NA) %>%
        dplyr::rename(total = "SumOfTotal.number") %>%
        process_karoo()
    
    karoo2 <- system.file("extdata", "sanparks/peggym.1050.1-Karoo2006.txt",
                          package = "MATSS", mustWork = TRUE) %>% 
        read.delim() %>%
        dplyr::mutate(Date = as.Date(.data$Date)) %>%
        dplyr::mutate(Species = replace(as.character(.data$Species), .data$Species == "kl", "kp"),
                      Species = replace(as.character(.data$Species), .data$Species == "?", "gr")) %>%
        dplyr::left_join(species, by = c("Species" = "Species")) %>%
        dplyr::rename(Latitude = "Lat", Longitude = "Long", total = "Total") %>%
        dplyr::select(c("Date", "Common.name", "total", "target", "Latitude", "Longitude"))
    
    karoo3 <- system.file("extdata", "sanparks/peggym.1049.1-karoo2008.txt",
                          package = "MATSS", mustWork = TRUE) %>%
        read.delim() %>%
        dplyr::rename(Common.name = "CONTROL", total = "TOTAL") %>%
        process_karoo()
    
    karoo4 <- system.file("extdata", "sanparks/peggym.1051.1-Karoo09.txt",
                          package = "MATSS", mustWork = TRUE) %>%
        read.delim() %>%
        dplyr::rename(Common.name = "CONTROL", total = "TOTAL") %>%
        process_karoo()
    
    #Combine cleaned tables and do more cleaning
    karoo_raw <- rbind(karoo1, karoo2, karoo3, karoo4) %>%
        dplyr::filter(.data$target == 1) %>%
        dplyr::mutate(year = lubridate::year(.data$Date))
    
    covariates <- karoo_raw %>%
        dplyr::group_by(.data$year) %>%
        dplyr::summarize(effort = dplyr::n_distinct(.data$Date))
    
    abundance <- karoo_raw %>%
        dplyr::group_by(.data$year, .data$Common.name) %>%
        dplyr::summarize(total = sum(.data$total)) %>%
        tidyr::spread(.data$Common.name, .data$total, fill = 0) %>%
        dplyr::ungroup() %>%
        dplyr::select(-.data$year)
    
    metadata <- list(timename = "year", 
                     latitude = mean(karoo_raw$Latitude, na.rm = TRUE), 
                     longitude = mean(karoo_raw$Longitude, na.rm = TRUE),
                     is_community = TRUE, 
                     citation = paste("SANParks (2009) Karoo National Park Census Data.",
                                      "1994 - 2009 (South African National Park Data Repository: peggym.117.10)"), 
                     source_url = "http://dataknp.sanparks.org/sanparks/metacat/peggym.117.10/sanparks",
                     acknowledgements = paste("Data were provided by SANParks Regional Ecologists, in a",  
                                              "cooperative effort of the South African National Parks (SANParks) and the National",  
                                              "Center for Ecological Analysis and Synthesis (NCEAS)."))
    
    list(abundance = abundance, covariates = covariates, metadata = metadata)
    
}


#' @title get Kruger National Park ungulate data
#' 
#' Import and clean Kruger National Park abundance from data files
#' 
#' @return list of two dataframes (one with abundance data, the other with
#'   covariate data), and one list of metadata.
#'
#' @export

get_kruger_data <- function()
{
    path <- system.file("extdata", "sanparks/judithk.815.1-815.1.txt",
                        package = "MATSS", mustWork = TRUE)
    
    kruger_raw <- read.delim(path) %>%
        dplyr::group_by(.data$year, .data$Species) %>%
        dplyr::mutate(TOTAL = tidyr::replace_na(.data$TOTAL, 
                                                sum(.data$South, .data$Central, .data$North, .data$FarNorth, 
                                                    na.rm = TRUE))) %>%
        dplyr::select(.data$year, .data$Species, .data$TOTAL) %>%
        tidyr::spread(.data$Species, .data$TOTAL, fill = 0) %>%
        dplyr::ungroup()
    
    covariates <- dplyr::select(kruger_raw, .data$year)
    abundance <- dplyr::select(kruger_raw, -.data$year)
    metadata <- list(timename = "year", 
                     latitude = "-23.990032", longitude = "31.554869",
                     is_community = TRUE, 
                     citation = paste("SANParks (1997) Census totals for large herbivores in the Kruger National",  
                                      "Park summarized by year and region 1965-1997 (South African National Park Data",  
                                      "Repository: judithk.814.4)"), 
                     source_url = "http://dataknp.sanparks.org/sanparks/metacat/judithk.814.4/sanparks",
                     acknowledgements = paste("Data were provided by SANParks Regional Ecologists, in a", 
                                              "cooperative effort of the South African National Parks (SANParks) and the National", 
                                              "Center for Ecological Analysis and Synthesis (NCEAS)."))
    
    list(abundance = abundance, covariates = covariates, metadata = metadata)
}

