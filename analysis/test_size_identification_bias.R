## Check for size-related biases in taxonomic identification

## load data ----

# comm_king18 <- readr::read_csv("derived_data/foram_data_king18.csv") %>% 
#   mutate(fraction = factor(fraction, 
#                            levels = c("63-100", "100-125", "125-150", ">150")))

## Differences in proportion of taxa identified to genus among cumulative size fraction ----

# proportion of taxa identified to genus for each station and cumulative size fraction
prop_genus <- comm_king18 %>% 
  mutate(incidence = ifelse(raw_density > 0, 1, 0)) %>% 
  select(-raw_density, -density_50cm3) %>% 
  arrange(species, station) %>% 
  mutate(taxon_level = ifelse(stringr::str_detect(species, " sp."),
                              "genus",
                              "species")) %>% 
  select(-species) %>% 
  group_by(taxon_level, station, fraction) %>% 
  summarise(incidence = sum(incidence)) %>% 
  ungroup() %>% 
  group_by(station, fraction) %>% 
  mutate(proportion = incidence / sum(incidence)) %>% 
  filter(taxon_level == "genus") %>% 
  ungroup()

# test difference with anova using proportion in each station
anova <- aov(proportion ~ fraction, data = prop_genus)
par(mfrow = c(2, 2))
plot(anova) # ok
anova_sum <- summary(anova)[[1]]

# make label for plot
F <- anova_sum["fraction", "F value"]
df <- anova_sum[, "Df"]
pvalue <- anova_sum["fraction", "Pr(>F)"]
label <- paste0("*F*(", paste(df, collapse = ", "), ") = ", round(F, 2), 
                ", *p* = ", round(pvalue, 2))

# plot percent of taxa identified to genus (supplementary figure S1 in the paper)
plot_perc_genus <- ggplot(prop_genus, aes(x = fraction, y = proportion * 100)) +
  geom_boxplot(outliers = FALSE) +
  geom_point(aes(color = station), 
             position = position_jitter(width = 0.2, seed = 1), 
             shape = 21) +
  annotate(geom = "richtext", 
           x = Inf, y = Inf, hjust = 1, vjust = 1,
           label = label, label.colour = NA, fill = NA, size = 3) +
  theme_bw(base_size = 10) +
  labs(x = "Size fraction (&mu;m)",
       y = "% of taxa identified to genus level") +
  theme(axis.title.x = ggtext::element_markdown())

plot_perc_genus

# save plot
png("output/percent_genus.png", width = 10, height = 8, units = "cm", res = 300)
print(plot_perc_genus)
dev.off()

cairo_pdf("output/percent_genus.pdf", width = 10/2.54, height = 8/2.54)
print(plot_perc_genus)
dev.off()

tiff("output/percent_genus.tiff", width = 10, height = 8, units = "cm", res = 300)
print(plot_perc_genus)
dev.off()

# No identification problem due to size/age

## Species lost for each cumulative size fraction ----

# species presence/absence in each cumulative size fraction
species_loss <- comm_king18 %>% 
  # species cumulative densities
  combine_communities() %>% 
  # change fraction to factor
  mutate(fraction = factor(fraction, 
                           levels = paste(c(63, 100, 125, 150), "µm"),
                           labels = paste0(">", c(63, 100, 125, 150)))) %>% 
  # compute mean species density across stations
  group_by(species, fraction) %>% 
  summarise(mean_density = mean(raw_density, na.rm = TRUE)) %>% 
  ungroup() %>% 
  # presence/absence
  mutate(presence = ifelse(mean_density > 0, 1, 0)) %>% 
  select(-mean_density)

# compute number of species lost from the total for each cumulative fraction
species_loss %>% 
  # summarise number of species in each cumulative fraction
  group_by(fraction) %>% 
  summarise(n_species = sum(presence, na.rm = TRUE)) %>% 
  # compute number and % of lost species from the smallest fraction
  mutate(lost_species = n_species - max(n_species),
         percent_lost = abs(lost_species) / max(n_species) * 100)
# fraction n_species lost_species percent_lost
# >63             85            0         0   
# >100            81           -4         4.71
# >125            76           -9        10.6 
# >150            64          -21        24.7 

# plot heatmap of presence/absence (supplementary figure S5 in the paper)
plot_species_loss <- species_loss %>% 
  mutate(presence = factor(presence,
                           levels = c(0, 1),
                           labels = c("absent", "present"))) %>% 
  ggplot(aes(x = fraction, y = species, fill = presence)) +
  geom_tile(color = "black") +
  theme_bw(base_size = 10) +
  coord_fixed(ratio = 1/5, expand = FALSE) +
  scale_x_discrete(position = "top") +
  scale_y_discrete(limits = rev) +
  scale_fill_manual(values = c("red", "white")) +
  xlab("Size fraction (&mu;m)") +
  theme(axis.title.y = element_blank(),
        axis.title.x = ggtext::element_markdown(),
        axis.text.y = element_text(face = "italic", size = 7),
        legend.title = element_blank())

plot_species_loss

# save plot
png("output/species_loss.png", width = 12, height = 18, units = "cm", res = 300)
print(plot_species_loss)
dev.off()

cairo_pdf("output/species_loss.pdf", width = 12/2.54, height = 18/2.54)
print(plot_species_loss)
dev.off()

tiff("output/species_loss.tiff", width = 12, height = 18, units = "cm", res = 300)
print(plot_species_loss)
dev.off()

## clean environment ----

# keep comm_king18 and glacier_dist
rm(list = ls()[!ls() %in% c("comm_king18", "glacier_dist")])
