## Prepare foraminifera data for analyses

## load data ----

raw_data <- read_delim("data/KING18_density10cm3_0-1cm.csv", delim = ";")

## rearrange data and standardise to 50 cm3 ----

comm_king18 <- raw_data %>% 
  rename("fraction" = "fraction (µm)") %>% 
  # average the two sediment layers (0-0.5, 0.5-1)
  group_by(station, fraction) %>% 
  summarise(across(where(is.double), mean)) %>% 
  ungroup() %>% 
  # convert to long format
  tidyr::pivot_longer(cols = -c(station, fraction),
                      names_to = "species",
                      values_to = "density_10cm3") %>% 
  # compute raw density (need to round to integer)
  # compute density per 50 cm3
  mutate(raw_density = round(density_10cm3 / 10 * (pi * 4.5^2 * 1), 0),
         density_50cm3 = raw_density / (pi * 4.5^2 * 1) * 50,
         station = tolower(station),
         fraction = factor(fraction, 
                           levels = c("63-100", "100-125", "125-150", ">150"))) %>% 
  select(-density_10cm3) %>% 
  arrange(station, fraction)

# save in derived_data
write_csv(comm_king18, "derived_data/foram_data_king18.csv")

## clean environment ----
# keep comm_king18
rm(raw_data)
