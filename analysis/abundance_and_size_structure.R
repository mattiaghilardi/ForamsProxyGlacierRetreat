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
              family = "gamma")
})
# "GLM supported: delta_AICc = 0.01"
# "GLM supported: delta_AICc = 0.05"
# "GLM supported: delta_AICc = 0.33"
# "GLM supported: delta_AICc = 0.34"
par(mfrow = c(2, 2))
for (i in 1:4) {
  cat(paste0("GLM diagnostics >", names(fit_density)[i]), fill = TRUE, labels = paste0("\n(", i, "):"))
  plot(fit_density[[i]]$GLM)
}
for (i in 1:4) {
  print(summary(fit_density[[i]]$GLM))
}

# create vector with fractions
fraction <- names(density)

# extract p-values of slopes and adjusted R2 and make label
labels_density <- lapply(1:4, function(i) {
  s <- summary(fit_density[[i]]$GLM)
  p <- s$coefficients[2, 4]
  if (p >= 0.001) {
    p_lab <- paste("=", round(p, 3))
  } else {
    p_lab <- "< 0.001"
  }
  r2 <- 1 - (s$deviance/s$null.deviance)
  adj_r2 <- 1 - (s$df.null/s$df.residual) * (1 - r2)
  label <- paste0("*adjR<sup> 2</sup>* = ", round(adj_r2, 2), "<br>*p* ", p_lab)
  data.frame(fraction = fraction[i],
             p = p,
             adj_r2 = adj_r2,
             label = label)
}) %>%
  bind_rows() %>%
  mutate(fraction = factor(fraction, levels = names(density)))

# make predictions
newdata <- data.frame(glacier_dist = seq(min(glacier_dist$glacier_dist), 
                                         max(glacier_dist$glacier_dist), 
                                         length.out = 100))

preds <- lapply(fit_density, function(x) { 
  pred <- predict(x$GLM, newdata, type = "response", se = TRUE)
  pred <- cbind(newdata, pred)
  return(pred)
}) %>%
  bind_rows(.id = "fraction") %>%
  mutate(fraction = factor(fraction, levels = names(density)),
         lower = fit - (2 * se.fit),
         upper = fit + (2 * se.fit))

# plot density vs glacier distance coloured by fraction
density_plot <- density %>%
  bind_rows() %>%
  ggplot(aes(x = glacier_dist, colour = fraction, fill = fraction)) +
  geom_ribbon(data = preds, 
              aes(ymin = lower, ymax = upper), 
              colour = NA, 
              alpha = 0.2) + 
  geom_line(data = preds, aes(y = fit)) +
  geom_point(aes(y = tot_50cm3)) +
  geom_richtext(data = labels_density,
                aes(x = -Inf, 
                    y = Inf, 
                    label = label),
                hjust = 0,
                vjust = 1,
                label.colour = NA,
                fill = NA,
                colour = "black") +
  facet_grid(cols = vars(fraction), 
             labeller = as_labeller(function(x) paste0(">", x))) +
  scale_colour_viridis_d(direction = -1,
                         aesthetics = c("colour", "fill")) + 
  ylab("Abundance (ind. 50 cm<sup>-3</sup>)") +
  xlab("Distance (km)") +
  theme_bw() +
  theme(panel.grid = element_blank(),
        axis.text = element_text(colour = "black"),
        axis.title.y = ggtext::element_markdown(),
        legend.position = "none",
        strip.background = element_blank(),
        strip.text = element_text(face = "bold", size = 10))

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

# extract p-values of splines
labels_rel_density <- data.frame(fraction = factor(levels(density_sep_fraction$fraction),
                                                   levels = levels(density_sep_fraction$fraction)),
                                 p = summary(fit_rel_density)$s.table[, 4]) %>% 
  mutate(label = if_else(p >= 0.001, 
                         paste("*p* =", round(p, 3)), 
                         "*p* < 0.001"))

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
rel_density_plot <- ggplot(density_sep_fraction, aes(x = glacier_dist)) + 
  geom_ribbon(data = pred_fit_rel_density, 
              aes(ymin = lower, ymax = upper), 
              colour = NA,
              alpha = 0.2) +
  geom_line(data = pred_fit_rel_density, aes(y = fit)) +
  geom_point(aes(y = relative)) +
  geom_richtext(data = labels_rel_density,
                aes(x = -Inf, 
                    y = Inf, 
                    label = label),
                hjust = 0,
                vjust = 1,
                label.colour = NA,
                fill = NA) +
  facet_grid(cols = vars(fraction),
             labeller = as_labeller(function(x) paste(x, "µm"))) +
  labs(y = "Relative abundance", 
       x = "Distance (km)") +
  theme_bw() + 
  theme(panel.grid = element_blank(),
        axis.text = element_text(colour = "black"),
        strip.background = element_blank(),
        strip.text = element_text(face = "bold", size = 10))

rel_density_plot

## final plot (figure 2 in the paper) ----

# combine plots
final_density_plot <- density_plot / 
  rel_density_plot + 
  plot_annotation(tag_levels = 'A',
                  theme = theme(plot.margin = margin(0, 0, 0, 0))) & 
  theme(plot.tag.position = c(0, 0.98),
        plot.tag = element_text(face = "bold"))

# save plot
png("output/density_plot.png", width = 18, height = 15, units = "cm", res = 300)
print(final_density_plot)
dev.off()

cairo_pdf("output/density_plot.pdf", width = 18/2.54, height = 15/2.54)
print(final_density_plot)
dev.off()

tiff("output/density_plot.tiff", width = 18, height = 15, units = "cm", res = 300)
print(final_density_plot)
dev.off()

## clean environment ----

# keep comm_king18, glacier_dist and fraction
rm(list = ls()[!ls() %in% c("comm_king18", "glacier_dist", "fraction")])
