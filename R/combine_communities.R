#' Compute species densities of cumulative size fractions (>63, >100, >125, >150)
#'
#' @param data Raw foraminiferal densities 
#' 
#' @return A tibble
combine_communities <- function(data) {
  data %>% 
    select(-density_50cm3) %>%
    group_by(station, species) %>% # samples grouped by station and species
    mutate("63 µm" = sum(raw_density)) %>% # summed the densities of the 4 size fractions
    filter(fraction != "63-100") %>% # remove the 63-100 from the old dataset
    mutate("100 µm" = sum(raw_density)) %>% # summed the densities of the 3 size fractions
    filter(fraction != "100-125") %>% # remove the 100-125 from the old dataset
    mutate("125 µm" = sum(raw_density)) %>% # summed the densities of the 2 size fractions
    filter(fraction != "125-150") %>% # remove the 125-150 from the old dataset
    mutate("150 µm" = raw_density) %>% # what is left in the old raw density is only the >150
    select(-raw_density, -fraction) %>% # delete unnecessary columns
    ungroup() %>% 
    pivot_longer(cols = 3:6, 
                 names_to = "fraction",
                 values_to = "raw_density") # rearrange the dataset in long format
}
