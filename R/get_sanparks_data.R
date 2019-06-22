#' @title get Karoo ungulate data
#' 
#' Import and clean Karoo abundance from data files
#' 
#' @return list of two dataframes (one with abundance data, the other with
#'   covariate data), and one list of metadata.
#'
#' @importFrom utils read.delim
#' @export

get_karoo_data <- function() {
    
    species_path <- system.file("extdata", "sanparks/peggym.114.1-KarooNationalParkCensuscodes.txt",
                             package = "MATSS", mustWork = TRUE)
    #Better organize species table    
    species <- read.delim(species_path) %>%
        dplyr::mutate(target = c(0,1,rep(0,7),rep(1,2),0,1,0,1,0,0,1,0,1,0,1,1,0,0,rep(1,4)), 
                      Common.name = gsub("\\s+","",as.character(Common.name)), 
                      Species = tolower(as.character(Species))) %>%
        rbind(c("BlackRhino","br",1), c("Burchellszebra","bz",1),c("Blesbok","bl",1))
    
    #Data tables are all formatted slightly differently
    data_path <- system.file("extdata", "sanparks/peggym.116.1-KRNPTotals.txt",
                             package = "MATSS", mustWork = TRUE)
    karoo1 = read.delim(data_path) %>%
        dplyr::mutate(Date = as.Date(Date)) %>%
        dplyr::mutate(Common.name = gsub("\\s+","",Common.name)) %>% 
        dplyr::left_join(species, by = c("Common.name" = "Common.name")) %>%
        dplyr::mutate(Latitude = NA, Longitude = NA) %>%
        dplyr::rename(total = "SumOfTotal.number") %>%
        dplyr::select(-Species)
    
    data_path2 <- system.file("extdata", "sanparks/peggym.1050.1-Karoo2006.txt",
                             package = "MATSS", mustWork = TRUE)
    karoo2 = read.delim(data_path2) %>%
        dplyr::mutate(Date = as.Date(Date)) %>%
        dplyr::mutate(Species = replace(as.character(Species), Species == "kl","kp"),
                      Species = replace(as.character(Species), Species == "?","gr")) %>%
        dplyr::left_join(species, by = c("Species" = "Species")) %>%
        dplyr::rename(Latitude = "Lat", Longitude = "Long",total = "Total") %>%
        dplyr::select(c("Date","Common.name","total","target","Latitude","Longitude"))
    
    data_path3 <- system.file("extdata", "sanparks/peggym.1049.1-karoo2008.txt",
                              package = "MATSS", mustWork = TRUE)
    karoo3 = read.delim(data_path3) %>%
        dplyr::mutate(Date = as.Date(Date)) %>%
        dplyr::rename(Common.name = "CONTROL", total = "TOTAL") %>%
        dplyr::mutate(Common.name = gsub("\\s+","",Common.name)) %>%
        dplyr::mutate(Common.name = replace(as.character(Common.name), 
                                            Common.name == "RedHartebeest","Redhartebeest"),
                      Common.name = replace(as.character(Common.name), 
                                            Common.name == "Mtnzebra","Mountainzebra"),
                      Common.name = replace(as.character(Common.name), 
                                            Common.name == "MtReedbuck","Mountainreedbuck")) %>%
        dplyr::left_join(species, by = c("Common.name" = "Common.name")) %>%
        dplyr::select(c("Date","Common.name","total","target","Latitude","Longitude"))
    
    data_path4 <- system.file("extdata", "sanparks/peggym.1051.1-Karoo09.txt",
                              package = "MATSS", mustWork = TRUE)
    karoo4 = read.delim(data_path4) %>%
        dplyr::mutate(Date = as.Date(Date)) %>%
        dplyr::rename(Common.name = "CONTROL", total = "TOTAL") %>%
        dplyr::mutate(Common.name = gsub("\\s+","",Common.name)) %>%
        dplyr::mutate(Common.name = replace(as.character(Common.name), 
                                            Common.name == "RedHartebeest","Redhartebeest"),
                      Common.name = replace(as.character(Common.name), 
                                            Common.name == "Mtnzebra","Mountainzebra"),
                      Common.name = replace(as.character(Common.name), 
                                            Common.name == "MtReedbuck","Mountainreedbuck")) %>%
        dplyr::left_join(species, by = c("Common.name" = "Common.name")) %>%
        dplyr::select(c("Date","Common.name","total","target","Latitude","Longitude"))
    
    #Combine cleaned tables and do more cleaning
    karoo_raw <- rbind(karoo1,karoo2,karoo3,karoo4) %>%
        dplyr::filter(target==1) %>%
        dplyr::mutate(year = lubridate::year(Date))
        
    covariates <- karoo_raw %>%
        dplyr::group_by(year) %>%
        dplyr::summarize(effort = dplyr::n_distinct(Date))
    
    abundance <- karoo_raw %>%
        dplyr::group_by(year, Common.name) %>%
        dplyr::summarize(total = sum(total)) %>%
        tidyr::spread(Common.name, total, fill = 0) %>%
        dplyr::ungroup() %>%
        dplyr::select(-year)
    
    metadata <- list(timename = "year", latitude = mean(karoo_raw$Latitude,na.rm=TRUE), 
                     longitude = mean(karoo_raw$Longitude,na.rm=TRUE),
                     source = "SANParks (2009) Karoo National Park Census Data. 1994 - 2009 
                     (South African National Park Data Repository: peggym.117.10)", 
                     source_url = "http://dataknp.sanparks.org/sanparks/metacat/peggym.117.10/sanparks",
                     acknowledgements = "Data were provided by SANParks Regional Ecologists, in a 
                     cooperative effort of the South African National Parks (SANParks) and the National 
                     Center for Ecological Analysis and Synthesis (NCEAS).")
    
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

get_kruger_data <- function() {
    
data_path <- system.file("extdata", "sanparks/judithk.815.1-815.1.txt",
                             package = "MATSS", mustWork = TRUE)

kruger_raw <- read.delim(data_path) %>%
    dplyr::group_by(year,Species) %>%
    dplyr::mutate(TOTAL = tidyr::replace_na(TOTAL, sum(South,Central,North,FarNorth,na.rm=TRUE))) %>%
    dplyr::select(year,Species,TOTAL) %>%
    tidyr::spread(Species, TOTAL, fill = 0) %>%
    dplyr::ungroup()

covariates <- kruger_raw %>%
    dplyr::select(year)

abundance <- kruger_raw %>%
    dplyr::select(-year)

metadata <- list(timename = "year", latitude = "-23.990032", longitude = "31.554869",
                 source = "SANParks (1997) Census totals for large herbivores in the Kruger National 
                 Park summarized by year and region 1965-1997 (South African National Park Data 
                 Repository: judithk.814.4) ", 
                 source_url = "http://dataknp.sanparks.org/sanparks/metacat/judithk.814.4/sanparks",
                 acknowledgements = "Data were provided by SANParks Regional Ecologists, in a 
                 cooperative effort of the South African National Parks (SANParks) and the National 
                 Center for Ecological Analysis and Synthesis (NCEAS).")

list(abundance = abundance, covariates = covariates, metadata = metadata)

}

