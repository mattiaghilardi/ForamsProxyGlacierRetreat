## Make heatmaps with species relative abundance for each cumulative size fraction

## load data ----

# comm_king18 <- readr::read_delim("data/foram_data_king18.csv", delim = ";")
# glacier_dist <- readr::read_delim("derived_data/glacier_dist.csv", delim = ";")

## data preparation ----

# create data frame
comm_heatmaps <- comm_king18 %>% 
  # species cumulative densities
  combine_communities() %>%
  # add column with distance from glacier front
  # need this to rearrange the stations for plotting
  left_join(glacier_dist %>% select(1, 4)) %>% 
  # compute relative abundance
  group_by(fraction, station) %>% 
  mutate(
    fraction = factor(fraction,
                      levels = paste(c(63, 100, 125, 150), "µm"),
                      labels = paste0(">", c(63, 100, 125, 150), " µm")),
    station = gsub("s", "S", station),
    rel_abund = raw_density/sum(raw_density)*100, # relative abundances
    # create factor palette and assign each row to an interval
    rel_abund_f = cut(rel_abund, 
                      breaks = c(min(rel_abund), 5, 10, 20, 30, 50, 70, max(rel_abund)),
                      labels = c("<5%", "5-10%", "10-20%", "20-30%", "30-50%", "50-70%", ">70%"), 
                      include.lowest = TRUE ),
    rel_abund = na_if(rel_abund, 0) # replace 0 with NA
  ) %>%
  ungroup() %>% 
  group_by(fraction, species) %>% # important to filter and keep also the abundances for the species <5%
  filter(any(rel_abund >= 5)) %>% # keep only the species >=5% in at least one station
  ungroup()

## plot (supplementary figure S2 in the paper) ----

heatmaps <- ggplot(comm_heatmaps, 
       aes(x = forcats::fct_reorder(station, glacier_dist), 
           y = species)) + 
  geom_tile(aes(fill = rel_abund_f)) +
  scale_fill_manual("Relative\nabundances", 
                    values = c("white", RColorBrewer::brewer.pal(6, "Blues"))) +
  geom_text(aes(label = round(rel_abund, 2)), size = 2.5) + 
  coord_cartesian(expand = FALSE) +
  facet_grid(rows = "fraction",scales = "free", space = "free") +
  theme(axis.title = element_blank(),
        legend.title = element_text(size = 9, hjust = 0.5, face = "bold"),
        legend.text = element_text(size = 8),
        axis.text.y = element_text(size = 8, face = "italic", color = "black"),
        axis.text.x = element_text(size = 8, color = "black"),
        legend.position = "bottom",
        legend.box.just = "center", 
        panel.border = element_rect(colour = "black", fill = NA, linewidth = 1),
        strip.background = element_blank(),
        strip.text = element_text(face = "bold", size = 12),
        plot.margin = margin(0.1, 0.1, 0.1, 0.1, "cm"))

# save plot
png("output/heatmaps.png", width = 18, height = 22, units = "cm", res = 300)
print(heatmaps)
dev.off()

cairo_pdf("output/heatmaps.pdf", width = 18/2.54, height = 22/2.54)
print(heatmaps)
dev.off()

tiff("output/heatmaps.tiff", width = 18, height = 22, units = "cm", res = 300)
print(heatmaps)
dev.off()

## clean environment ----
rm(comm_heatmaps, heatmaps)
