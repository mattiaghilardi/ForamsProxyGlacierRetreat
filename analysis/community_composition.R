## Build NMDS and model NMDS1 in relation to the
## distance from the glacier front

# ## load data ----
# 
# comm_king18 <- readr::read_delim("data/foram_data_king18.csv", delim = ";")
# glacier_dist <- readr::read_delim("derived_data/glacier_dist.csv", delim = ";")

## prepare data for NMDS ----

# create data frame
comm_NMDS <- comm_king18 %>% 
  # species cumulative densities
  combine_communities() %>%
  # rearrange to have a column for each species
  pivot_wider(names_from = "species", values_from = "raw_density")
 
# create vectors of fractions and stations
# fraction <- comm_NMDS$fraction
stations <- comm_NMDS$station

# combine station and fraction and use as row names
comm_NMDS <- comm_NMDS %>%
  mutate(station = paste(station, fraction, sep = "_"))%>%
  column_to_rownames(var = "station") %>%
  select(-fraction)

## NMDS analysis ----

# NMDS KING18 community
NMDS_KING18 <- vegan::metaMDS(comm_NMDS, k = 2)

# to check the analysis
stressplot <- vegan::stressplot(NMDS_KING18) # Stress=0.085

# base plot for NMDS 
plot(NMDS_KING18)

# now plot NMDS using ggplot

# extract the site scores and convert to a data.frame
data_scores <- as.data.frame(vegan::scores(NMDS_KING18)$site) 

# add a column with site names
data_scores$station <- stations 
data_scores$fraction <- fraction

# hull values
hull_fractions <- lapply(unique(fraction), function(i) {
  data_scores[data_scores$fraction == i, ][chull(data_scores[data_scores$fraction == i, c("NMDS1", "NMDS2")]), ]  
}) 

hull_data <- bind_rows(hull_fractions)

# calculate hull volume (surface)
hull_volume <- lapply(1:4, function(i) 
  geometry::convhulln(hull_fractions[[i]][, 1:2], output.options = "FA")$vol)
names(hull_volume) <- fraction

# reorder factor levels for plotting
data_scores$fraction <- factor(data_scores$fraction, levels = paste(c(63, 100, 125, 150), "µm"))
hull_data$fraction <- factor(hull_data$fraction, levels = paste(c(63, 100, 125, 150), "µm"))

# add glacier distance
data_scores <- data_scores %>%
  left_join(glacier_dist %>%
              select(station, glacier_dist)) %>%
  mutate(station = gsub("s", "S", station))

# NMDS plot with ggplot using polygons to identify the same size fraction
pNMDS <- ggplot() +
  geom_polygon(data = hull_data,
               aes(x = NMDS1, y = NMDS2, 
                   fill = fraction, colour = fraction, group = fraction),
               alpha = 0.20) +
  geom_point(data = data_scores,
             aes(x = NMDS1, y = NMDS2, 
                 shape = forcats::fct_reorder(station, glacier_dist), 
                 colour = fraction), 
             size = 2) + 
  scale_shape_manual("Station", values = c(5, 15, 1, 16, 2, 17, 8, 18, 4)) +
  scale_fill_viridis_d("Size<br>fraction (&mu;m)", 
                       labels = c(">63",">100",">125",">150"),
                       direction = -1,
                       aesthetics = c("fill", "color")) +
  coord_equal() +
  theme_bw()  +
  theme(panel.grid = element_blank(),
        axis.text = element_text(colour = "black", size = 9),
        axis.title  = element_text(colour = "black", size = 10),
        legend.title = element_markdown(face = "bold", size = 8, hjust = 0.5),
        legend.text = element_text(size = 8),
        legend.position = "right",
        legend.direction = "vertical",
        legend.box = "horizontal",
        legend.key.size = unit(0.4, "cm"))
pNMDS

## GAM NMDS1 vs distance from the glacier ----
nmds_data <- split(data_scores, data_scores$fraction)

fit_nmds <- lapply(nmds_data, function(x) { 
  fit_glm_gam(glm_formula = NMDS1 ~ glacier_dist,
              gam_formula = NMDS1 ~ s(glacier_dist, k = 3, bs = "cr"),
              data = x,
              family = "gaussian")
})

for (i in 1:4) {
  print(summary(fit_nmds[[i]]$GLM))
}

plot(fit_nmds[[1]]$GLM, ask = FALSE)
plot(fit_nmds[[2]]$GLM, ask = FALSE)
plot(fit_nmds[[3]]$GLM, ask = FALSE)
plot(fit_nmds[[4]]$GLM, ask = FALSE)
# non-linearity --> use GAMs

for (i in 1:4) {
  print(summary(fit_nmds[[i]]$GAM))
}

for (i in 1:4) {
  gam.check(fit_nmds[[i]]$GAM)
}

# plot models
newdata <- data.frame(glacier_dist = seq(min(glacier_dist$glacier_dist), 
                                         max(glacier_dist$glacier_dist), 
                                         length.out = 100))

# colours
col <- viridisLite::viridis(4, direction = -1)

plots_nmds <- mapply(i = 1:4, j = c(1, 0, 0, 0), function(i,j) {
  plot_diversity(model = fit_nmds[[i]]$GAM,
                 data = nmds_data[[i]],
                 newdata = newdata,
                 y = "NMDS1", 
                 title = paste0(">", names(nmds_data)[i]), 
                 xlab = "Distance (km)", 
                 ylab = "NMDS1",
                 color = col[i],
                 fill = col[i]) +
    theme(axis.text = element_text(size = 9),
          axis.title.x = element_text(size = 10),
          axis.title.y = if(j == 0) element_blank()
                          else element_text(size = 10),
          plot.title = element_text(size = 10))
}, SIMPLIFY = FALSE)

## final plot ----

# combine plots
plots_nmds <- patchwork::wrap_plots(plots_nmds, nrow = 1) + 
  patchwork::plot_annotation(theme = theme(plot.margin = margin(0, 0, 0, 0)))

final_plot_nmds <- 
  (free(pNMDS) + 
     theme(plot.margin = margin(0, 0, 0, 0))) / 
  plots_nmds + 
  plot_layout(heights = c(0.6, 0.4)) + 
  plot_annotation(tag_levels = list(c("a", "b")),
                  theme = theme(plot.margin = margin(0, 0, 0, 0))) & 
  theme(plot.tag = element_text(face = "bold", hjust = 1, vjust = 0))

# save plot
png("output/nmds.png", width = 18, height = 15, units = "cm", res = 300)
print(final_plot_nmds)
dev.off()

cairo_pdf("output/nmds.pdf", width = 18/2.54, height = 15/2.54)
print(final_plot_nmds)
dev.off()

tiff("output/nmds.tiff", width = 18, height = 15, units = "cm", res = 300)
print(final_plot_nmds)
dev.off()

## clean environment ----
# keep comm_king18, glacier_dist, fraction and col
rm(list = ls()[!ls() %in% c("comm_king18", "glacier_dist", "fraction", "col")])
