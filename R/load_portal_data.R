# portal_abundance <- load('data/portal/portal_abundance_table.RDS')
# RMD's computer can't load portal_abundance_table.RDS:
# 
# > portal_abundance <- load('data/portal/portal_abundance_table.RDS')
# Error in load("data/portal/portal_abundance_table.RDS") : 
#     bad restore file magic number (file may be corrupted) -- no data loaded
# In addition: Warning message:
#     file ‘portal_abundance_table.RDS’ has magic number 'X'
# Use of save versions prior to 2 is deprecated 
# > 

# from LDA-kratplots - includes a number of decisions we might want to adjust

library(dplyr)
library(portalr)
library(RCurl)

get_rodent_lda_data <- function(time_or_plots, treatment, type) {
    # selected_species = c('BA','DM','DO','DS','NA','OL','OT','PB','PE','PF','PH','PI','PL','PM','PP','RF','RM','RO','SF','SH','SO')
    
    if (tolower(time_or_plots) == 'plots') {
        length = 'all'
        startperiod = 118
        standardeffort = 8
    } else if(tolower(time_or_plots) == 'time') {
        length = 'longterm'
        startperiod = 1
        standardeffort = 4
    }
    
    dat <- abundance(path = "repo", clean = FALSE, 
                     level = 'Plot', type = type, plots = length, 
                     unknowns = FALSE, fill_incomplete = F, shape = 'crosstab',
                     time = 'period', effort = TRUE, min_plots = 0)
    selected_species = colnames(dat)[5:ncol(dat)]
    
    if (treatment == 'exclosure') {
        dat2 <- dat %>%
            filter(treatment == 'exclosure', period %in% startperiod:436,
                   ntraps >= 1) %>%
            mutate(effort = 1) %>%
            group_by(period) %>%
            summarise_at(c(selected_species, 'effort'), sum)
    } else if (treatment == 'control'){
        dat2 <- dat %>%
            filter(plot %in% c(2,4,8,11,12,14,17,22), 
                   period %in% startperiod:436,
                   ntraps >= 1) %>%
            mutate(effort = 1) %>%
            group_by(period) %>%
            summarise_at(c(selected_species, 'effort'), sum)
    }
    
    datsums = vector(length =nrow(dat2))
    
    for(i in 1:nrow(dat2)) {
        thiseffort = dat2[i, 'effort']
        for (j in 1:length(selected_species)) {
            dat2[i,selected_species[j]] = round((dat2[i, selected_species[j]] / thiseffort) * standardeffort)
        }
        datsums[i] = sum(dat2[i,selected_species])
    }
    
    dat2 = dat2[ which(datsums >= 1), c('period', selected_species)]
    
    moondat = read.csv(text=getURL("https://raw.githubusercontent.com/weecology/PortalData/master/Rodents/moon_dates.csv"),stringsAsFactors = F)
    moondat$date = as.Date(moondat$censusdate)
    
    period_dates = filter(moondat,period %in% dat2$period) %>% select(period,date, newmoonnumber)
    
    newmoon_offset = min(period_dates$newmoon) - 1
    
    dat2 <- dat2 %>%
        mutate(date = period_dates$date, 
               newmoon = period_dates$newmoonnumber,
               timestep = newmoon - newmoon_offset)
    
    
    return(dat2)
}


rodent_data = get_rodent_lda_data(time_or_plots = 'time', treatment = 'control', type = 'granivores')
rodent_data = rodent_data %>%
    select(-period, 
           -newmoon)
write.csv(rodent_data, 'data/portal/portal_controls_data.csv', row.names = F)