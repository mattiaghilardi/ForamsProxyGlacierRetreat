## Plot traits in relation to the distance from the glacier front

## load data ----

# comm_king18 <- readr::read_csv("derived_data/foram_data_king18.csv")
traits <- readr::read_delim("data/species_traits.csv", delim = ";")
# glacier_dist <- readr::read_delim("derived_data/glacier_dist.csv", delim = ";")

## data preparation ----

# create data frame
comm_relative <- comm_king18 %>% 
  # species cumulative densities
  combine_communities() %>%
  # calculate relative abundances for each species
  group_by(station, fraction) %>%
  mutate(rel_abund = raw_density/sum(raw_density)*100) %>%
  ungroup()

# convert the traits to factors
traits_factor <- traits %>%
  mutate(across(.cols = 2:7, .fns = as.factor))

# add functional traits and distance from the glacier
comm_traits <- comm_relative %>% 
  left_join(traits_factor) %>%
  left_join(glacier_dist %>% select(1, 4))

# rearrange dataset in long format
comm_traits <- comm_traits %>%
  pivot_longer(cols = 6:11, names_to = "trait", values_to = "value")

# summarise relative abundances for each trait and fraction
comm_traits <- comm_traits %>%
  group_by(station, fraction, glacier_dist, trait, value) %>%
  summarise(rel_abund = sum(rel_abund)) %>% 
  ungroup()

# adjust order of fractions, traits names and use capital letter for stations
comm_traits <- comm_traits %>%
  mutate(fraction = factor(fraction,
                           levels = paste(c(63, 100, 125, 150), "µm"),
                           labels = paste0(">", c(63, 100, 125, 150), " µm")),
         trait = factor(trait, 
                        levels = c("shell", 
                                   "chamber_number", 
                                   "pores", 
                                   "shape",
                                   "teeth",
                                   "symmetry"),
                        label = c("Test material",
                                  "Chamber number",
                                  "Pores",
                                  "Test shape",
                                  "Teeth",
                                  "Test symmetry")),
         station = gsub("s", "S", station))

# convert data frame to list: split by trait
comm_traits <- split(comm_traits, comm_traits$trait)

## plot (supplementary figure S3 in the paper) ----

# list with arguments to pass to the plots
traits_labels <- list("Test material" = list(labels = c("Agglutinated", "Calcareous"),
                                             colors = c("#66CCCC", "grey")),
                      "Chamber number" = list(labels = c("Monothalamous", "Polythalamous"),
                                              colors = c("#66CCCC", "grey")),
                      "Pores" = list(labels = c("Absence", "Presence"),
                                     colors = c("#66CCCC", "grey")),
                      "Test shape" = list(labels = c("Elongated","Irregular", "Ovoid","Rounded"),
                                          colors = c("#66CCCC","#00FFCC","#6699FF", "grey")),
                      "Teeth" = list(labels = c("Absence","Single-tooth", "Teeth-like\ntubercles"),
                                     colors = c("#66CCCC","gray","#6699FF")),
                      "Test symmetry" = list(labels = c("Asymmetrycal","Symmetrycal"),
                                             colors = c("#66CCCC", "grey")))

# make plots
traits_plots <- lapply(1:6, function(i){
  p <- ggplot(comm_traits[[i]],
              aes(fill = value,
                  y = rel_abund, 
                  x = forcats::fct_reorder(station, glacier_dist))) +
    geom_bar(position = "stack", stat = "identity") +
    labs(y = "Relative abundance (%)", fill = names(comm_traits)[i]) +
    facet_wrap(facets = "fraction", nrow = 1) +
    scale_fill_manual(values = traits_labels[[i]]$colors, 
                      labels = traits_labels[[i]]$labels) +
    theme_bw() +
    theme(axis.title.y = element_text(size = 8),
          axis.title.x = element_blank(),
          axis.text = element_text(colour = "black", size = 6),
          legend.title = element_text(face = "bold", size = 8),
          legend.text = element_text(size = 7),
          legend.position = "right",
          legend.box.spacing = unit(0.1, "cm"),
          panel.grid = element_blank(),
          strip.background = element_blank(),
          strip.text = element_text(face = "bold", size = 9)) +
    coord_cartesian(expand = FALSE)
  
  # keep strip text only in first plot
  if (i != 1) p <- p +
      theme(strip.background = element_blank(),
            strip.text = element_blank())
  
  # keep x axis text only in last plot
  if (i != 6) p <- p +
      theme(axis.text.x = element_blank(),
            axis.ticks.x = element_blank())
  
  p
})

# combine plots
final_traits_plot <- patchwork::wrap_plots(traits_plots, ncol = 1) & 
  theme(legend.justification = "left",
        plot.margin = margin(0.1, 0.1, 0.1, 0.1, "cm"))

# save plot
png("output/traits_distribution.png", width = 18, height = 20, units = "cm", res = 300)
print(final_traits_plot)
dev.off()

cairo_pdf("output/traits_distribution.pdf", width = 18/2.54, height = 20/2.54)
print(final_traits_plot)
dev.off()

tiff("output/traits_distribution.tiff", width = 18, height = 20, units = "cm", res = 300)
print(final_traits_plot)
dev.off()

## clean environment ----
rm("comm_traits", "comm_relative", "traits_labels", 
   "traits_plots", "final_traits_plot", "traits_factor")
