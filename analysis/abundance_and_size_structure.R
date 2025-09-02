## Model foraminiferal abundance and size structure
## in relation to the distance from the glacier front

## load data ----

# comm_king18 <- readr::read_csv("derived_data/foram_data_king18.csv")
# glacier_dist <- readr::read_delim("derived_data/glacier_dist.csv", delim = ";")

## density of cumulative size fractions ----

# create data frame
density <- comm_king18 %>% 
  # species cumulative densities
  combine_communities() %>%
  # summarise for each station and fraction
  group_by(station, fraction) %>%
  summarise(raw_tot = sum(raw_density)) %>%
  # core radius is 4.5
  # compute density per 1 cm^3 and multiply by 50
  mutate(tot_50cm3 = raw_tot/(pi*4.5^2*1)*50,
         fraction = factor(fraction, 
                           levels = paste(c(63, 100, 125, 150), "µm"))) %>%
  ungroup() %>% 
  # add column with distance from glacier front
  left_join(glacier_dist %>% select(1, 4))

# convert data frame to list: split by fraction
density <- split(density, density$fraction)

# fit models
fit_density <- lapply(density, function(x) { 
  fit_glm_gam(glm_formula = tot_50cm3 ~ glacier_dist,
              gam_formula = tot_50cm3 ~ s(glacier_dist, k = 3, bs = "cr"),
              data = x,
              family = "gaussian")
})

for (i in 1:4) {
  print(summary(fit_density[[i]]$GLM))
}

# create vector with fractions
fraction <- names(density)

# extract p-values of slopes
pvalue <- lapply(1:4, function(i) {
  s <- summary(fit_density[[i]]$GLM)
  p <- s$coefficients[2, 4]
  p <- round(p, 2)
  data.frame(fraction = fraction[i],
             p = p)
}) %>%
  bind_rows()

# plot density vs glacier distance coloured by fraction
density_plot <- density %>%
  bind_rows() %>%
  ggplot(aes(x = glacier_dist, y = tot_50cm3, colour = fraction)) +
  geom_point() +
  geom_richtext(data = pvalue,
                aes(x = 3, 
                    y = c(6000, 5600, 5200, 4800), 
                    label = paste("<i>p</i> =", p), 
                    color = fraction),
                label.colour = NA,
                fill = NA,
                show.legend = FALSE) +
  scale_colour_viridis_d("Size fraction (&mu;m)",
                         direction = -1,
                         labels = c(">63", ">100", ">125", ">150")) +
  scale_y_continuous(breaks = seq(0, 6000, 1000)) +
  ylab("Abundance (ind. 50 cm<sup>-3</sup>)") +
  xlab("Distance (km)") +
  theme_bw() +
  theme(panel.grid = element_blank(),
        axis.text = element_text(colour = "black"),
        axis.title.y = ggtext::element_markdown(),
        legend.position = "top",
        legend.title = ggtext::element_markdown(hjust = 0.5, size = 11),
        legend.title.position = "top",
        legend.text = element_text(size = 9),
        legend.key.width = unit(0.6, "cm"),
        legend.key.spacing = unit(0.1, "cm"),
        legend.box.spacing = unit(0.15, "cm"))

density_plot

## relative density of individual size fractions ----

# create data frame
density_sep_fraction <- comm_king18 %>%
  mutate(fraction = factor(fraction, 
                           levels = c("63-100", "100-125", "125-150", ">150"))) %>%
  # first compute total density for each station
  group_by(station) %>%
  mutate(total63 = sum(raw_density)) %>%
  # now compute relative density for each fraction in each station
  group_by(station, fraction) %>%
  mutate(partial = sum(raw_density), 
         relative = partial/total63) %>%
  select(station, fraction, relative) %>%
  # add glacier distance
  left_join(select(glacier_dist, station, glacier_dist)) %>%
  unique()

# fit GAM to relative density
fit_rel_density <- gam(relative ~ fraction + s(glacier_dist, k = 3, bs = "cr", by = fraction), 
                       family = "betar", data = density_sep_fraction)

summary(fit_rel_density)
gam.check(fit_rel_density)

# make predictions
nd <- expand_grid(glacier_dist = seq(min(glacier_dist$glacier_dist), 
                                     max(glacier_dist$glacier_dist), 
                                     length.out = 100),
                  fraction = unique(density_sep_fraction$fraction))

pred_fit_rel_density <- predict(fit_rel_density, nd, type = "response", se = TRUE)
pred_fit_rel_density <- cbind(nd, pred_fit_rel_density )
pred_fit_rel_density$lower <- pred_fit_rel_density$fit - (2 * pred_fit_rel_density$se.fit)
pred_fit_rel_density$upper <- pred_fit_rel_density$fit + (2 * pred_fit_rel_density$se.fit)

# plot relative density vs glacier distance for each fraction
rel_density_plot <- ggplot(mapping = aes(x = glacier_dist)) +
  geom_point(data = density_sep_fraction, aes(y = relative, shape = fraction)) +
  geom_line(data = pred_fit_rel_density, aes(y = fit, linetype = fraction)) +
  labs(shape = "Size fraction (&mu;m)", 
       linetype = "Size fraction (&mu;m)", 
       y = "Relative abundance", 
       x = "Distance (km)") +
  scale_shape_manual(values = c(16, 17, 1, 2)) +
  scale_linetype_manual(values = c(1, 2, 3, 4)) +
  theme_bw() +
  theme(panel.grid = element_blank(),
        axis.text = element_text(colour = "black"),
        legend.position = "top",
        legend.title = ggtext::element_markdown(hjust = 0.5, size = 11), 
        legend.title.position = "top",
        legend.text = element_text(size = 9),
        legend.key.width = unit(0.6, "cm"),
        legend.key.spacing = unit(0.1, "cm"),
        legend.box.spacing = unit(0.15, "cm"))

rel_density_plot

## final plot (figure 2 in the paper) ----

# combine plots
final_density_plot <- density_plot + 
  theme(plot.margin = margin(0.1, 0.15, 0.1, 0.1, "cm")) + 
  rel_density_plot + 
  theme(plot.margin = margin(0.1, 0.1, 0.1, 0.15, "cm")) + 
  plot_annotation(tag_levels = 'a',
                  theme = theme(plot.margin = margin(0, 0, 0, 0))) & 
  theme(plot.tag.position = c(0.03, 0.98),
        plot.tag = element_text(face = "bold"))

# save plot
png("output/density_plot.png", width = 18, height = 11, units = "cm", res = 300)
print(final_density_plot)
dev.off()

cairo_pdf("output/density_plot.pdf", width = 18/2.54, height = 11/2.54)
print(final_density_plot)
dev.off()

tiff("output/density_plot.tiff", width = 18, height = 11, units = "cm", res = 300)
print(final_density_plot)
dev.off()

## clean environment ----

# keep comm_king18, glacier_dist and fraction
rm(list = ls()[!ls() %in% c("comm_king18", "glacier_dist", "fraction")])
