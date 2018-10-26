library(dplyr)

process_kartzinel_data <- function(kart_data) {
    
    # Smammal surveys from 3 sets of 3 blocks at different sites along a
    # rainfall gradient. Blocks are part of experimental exclosures;
    # here using control ("open") data.

    kartzinel_2014 <- read.table('data/kartzinel-et-al-2014.txt',
                               stringsAsFactors = F, head = T, sep = "\t",
                               na.strings = "")

    # filter to open for now
 
    kartzinel_2014 <- filter(kartzinel_2014, TREATMENT == 'OPEN') 
    
    kartzinel_2014_south_cov <- kartzinel_2014 %>%
        select(DATE, SURVEY, SITE) %>%
        filter(SITE == "SOUTH") %>%
        mutate(DATE = as.Date(DATE, '%d-%B-%y')) %>%
        group_by(SURVEY) %>%
        summarize(DATE = min(DATE)) %>%
        ungroup()
    
    kartzinel_2014_central_cov <- kartzinel_2014 %>%
        select(DATE, SURVEY, SITE) %>%
        filter(SITE == "CENTRAL") %>%
        mutate(DATE = as.Date(DATE, '%d-%B-%y')) %>%
        group_by(SURVEY) %>%
        summarize(DATE = min(DATE)) %>%
        ungroup()
    
    
    kartzinel_2014_north_cov <- kartzinel_2014 %>%
        select(DATE, SURVEY, SITE) %>%
        filter(SITE == "NORTH") %>%
        mutate(DATE = as.Date(DATE, '%d-%B-%y')) %>%
        group_by(SURVEY) %>%
        summarize(DATE = min(DATE)) %>%
        ungroup()
    
    
    kartzinel_2014_south_rodents <- kartzinel_2014 %>%
        filter(!is.na(SPECIES), SPECIES != 'X', SITE == "SOUTH") %>%
        select(SURVEY, BLOCK, SPECIES, ID) %>%
    group_by(SURVEY, BLOCK, SPECIES) %>%
        distinct(ID) %>%
        ungroup() %>%
        group_by() %>%
        select(-BLOCK) %>%
        group_by(SURVEY, SPECIES) %>%
        summarize(count = n()) %>%
        ungroup()

    kartzinel_2014_south_rodents_table <- kartzinel_2014_south_rodents %>%
        tidyr::spread(SPECIES, count, fill = 0) %>%
        select(-SURVEY)
    
    kartzinel_2014_south <- list(kartzinel_2014_south_cov, kartzinel_2014_south_rodents_table)

    
    
    kartzinel_2014_central_rodents <- kartzinel_2014 %>%
        filter(!is.na(SPECIES), SPECIES != 'X', SITE == "CENTRAL") %>%
        select(SURVEY, BLOCK, SPECIES, ID) %>%
        group_by(SURVEY, BLOCK, SPECIES) %>%
        distinct(ID) %>%
        ungroup() %>%
        group_by() %>%
        select(-BLOCK) %>%
        group_by(SURVEY, SPECIES) %>%
        summarize(count = n()) %>%
        ungroup()
    
    kartzinel_2014_central_rodents_table <- kartzinel_2014_central_rodents %>%
        tidyr::spread(SPECIES, count, fill = 0) %>%
        select(-SURVEY)
    
    kartzinel_2014_central <- list(kartzinel_2014_central_cov, kartzinel_2014_central_rodents_table)
    
    kartzinel_2014_north_rodents <- kartzinel_2014 %>%
        filter(!is.na(SPECIES), SPECIES != 'X', SITE == "NORTH") %>%
        select(SURVEY, BLOCK, SPECIES, ID) %>%
        group_by(SURVEY, BLOCK, SPECIES) %>%
        distinct(ID) %>%
        ungroup() %>%
        group_by() %>%
        select(-BLOCK) %>%
        group_by(SURVEY, SPECIES) %>%
        summarize(count = n()) %>%
        ungroup()
    
    kartzinel_2014_north_rodents_table <- kartzinel_2014_north_rodents %>%
        tidyr::spread(SPECIES, count, fill = 0) %>%
        select(-SURVEY)
    
    kartzinel_2014_north <- list(kartzinel_2014_north_cov, kartzinel_2014_north_rodents_table)
    
    
    
    kartzinel_2014_tables <- list(kartzinel_2014_south, kartzinel_2014_central, kartzinel_2014_north)

    return(kartzinel_2014_tables)
    }