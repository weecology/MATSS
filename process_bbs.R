library(dplyr)
library(readr)
dat <- read_csv("~/data/breed-bird-survey/breed_bird_survey_region_codes.csv")
dat %>%
    filter(countrynum == 124, regioncode == 4) %>%
    write_csv("inst/extdata/subsampled/breed-bird-survey/breed_bird_survey_region_codes.csv")

dat <- read_csv("~/data/breed-bird-survey/breed_bird_survey_counts.csv")
dat %>%
    filter(countrynum == 124, statenum == 4) %>%
    head(n = 100) %>%
    write_csv("inst/extdata/subsampled/breed-bird-survey/breed_bird_survey_counts.csv")

dat <- read_csv("~/data/breed-bird-survey/breed_bird_survey_weather.csv")
dat %>%
    filter(countrynum == 124, statenum == 4) %>%
    head(n = 100) %>%
    write_csv("inst/extdata/subsampled/breed-bird-survey/breed_bird_survey_weather.csv")

dat <- read_csv("~/data/breed-bird-survey/breed_bird_survey_species.csv")
dat %>%
    filter(countrynum == 124, statenum == 4) %>%
    head(n = 100) %>%
    write_csv("inst/extdata/subsampled/breed-bird-survey/breed_bird_survey_weather.csv")
