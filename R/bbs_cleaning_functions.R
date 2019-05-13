#' Filter poorly sampled BBS species
#' 
#' Modified from https://github.com/weecology/bbs-forecasting/blob/master/R/forecast-bbs-core.R
#'
#' Removes waterbirds, shorebirds, owls, kingfishers, knightjars,
#' dippers. These species are poorly sampled due to their aquatic or
#' noctural nature. Also removes taxa that were either partially unidentified
#' (e.g. "sp.") or were considered hybrids (e.g. "A x B") or were listed as more
#' than one species (e.g. "A / B")
#'
#' @param df dataframe containing an species_id column
#'
#' @return dataframe, filtered version of initial dataframe
#' @export

filter_bbs_species <- function(df, species_table){
    
    is_unidentified = function(names) {
        #Before filtering, account for this one hybrid of 2 subspecies so it's kept
        names[names=='auratus auratus x auratus cafer']='auratus auratus'
        grepl('sp\\.| x |\\/', names)
    }
    
    valid_taxa = species_table %>%
        dplyr:: filter(!is_unidentified(species)) %>%
        dplyr::filter(aou > 2880) %>%
        dplyr::filter(aou < 3650 | aou > 3810) %>%
        dplyr::filter(aou < 3900 | aou > 3910) %>%
        dplyr::filter(aou < 4160 | aou > 4210) %>%
        dplyr::filter(aou != 7010)
    
    dplyr::filter(df, species_id %in% valid_taxa$aou)
}

#' Combine subspecies into their common species
#' 
#' Modified from https://github.com/weecology/bbs-forecasting/blob/master/R/forecast-bbs-core.R 
#'
#' @export
combine_subspecies = function(df, species_table){
    
    # Subspecies have two spaces separated by non-spaces
    subspecies_names = species_table %>%
        dplyr::filter(aou %in% unique(df$species_id)) %>%
        dplyr::pull(spanish_common_name) %>%
        grep(" [^ ]+ ", ., value = TRUE)
    
    subspecies_ids = species_table %>%
        dplyr::filter(spanish_common_name %in% subspecies_names) %>%
        dplyr::pull(aou)
    
    # Drop all but the first two words to get the root species name,
    # then find the AOU code
    new_subspecies_ids = species_table %>%
        dplyr::slice(match(stringr::word(subspecies_names, 1,2),
                           species_table$spanish_common_name)) %>%
        dplyr::pull(aou)
    
    # replace the full subspecies names with species-level names
    for (i in seq_along(subspecies_ids)) {
        df$species_id[df$species_id == subspecies_ids[i]] = new_subspecies_ids[i]
    }
    
    df_grouped <- df %>%
        dplyr::group_by_at(dplyr::vars(-abundance)) %>%
        dplyr::summarise(abundance = sum(abundance)) %>%
        dplyr::ungroup() %>%
        dplyr::distinct()
}
