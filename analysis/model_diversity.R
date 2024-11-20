## Model taxonomic and functional diversity in relation to the
## distance from the glacier front

# ## load data ----
# 
# results <- readr::read_rds("derived_data/diversity_results.rds")
# glacier_dist <- readr::read_delim("derived_data/glacier_dist.csv", delim = ";")

## add distance from glacier front to the data frames with the diversity metrics ----

results <- sapply(names(results), function(x) 
  bind_cols(results[[x]], glacier_dist = glacier_dist$glacier_dist),
  simplify = FALSE, USE.NAMES = TRUE)

## fit models ----

## taxonomic richness ----
fit_rich <- lapply(results, function(x) {
  fit_glm_gam(glm_formula = Richness ~ glacier_dist,
              gam_formula = Richness ~ s(glacier_dist, k = 3, bs = "cr"),
              data = x,
              family = "poisson")
})
# "GAM supported: delta_AICc = 13.4"
# "GAM supported: delta_AICc = 13.03"
# "GAM supported: delta_AICc = 14.21"
# "GAM supported: delta_AICc = 11.18"
par(mfrow = c(2, 2))
for (i in 1:4) {
  cat(paste0("GAM diagnostics >", names(fit_rich)[i]), fill = TRUE, labels = paste0("\n(", i, "):"))
  gam.check(fit_rich[[i]]$GAM)
}
for (i in 1:4) {
  cat(paste0("summary GAM >", names(fit_rich)[i]), fill = TRUE, labels = paste0("\n(", i, "):"))
  print(summary(fit_rich[[i]]$GAM))
}

## taxonomic evenness ----
fit_eve <- lapply(results, function(x) {
  fit_glm_gam(glm_formula = Evenness ~ glacier_dist,
              gam_formula = Evenness ~ s(glacier_dist, k = 3, bs = "cr"),
              data = x,
              family = "beta")
})
# "GLM supported: delta_AICc = 2.45"
# "GLM supported: delta_AICc = 3.43"
# "GLM supported: delta_AICc = 3.75"
# "GLM supported: delta_AICc = 0.18"
par(mfrow = c(2, 2))
for (i in 1:4) {
  cat(paste0("GLM diagnostics >", names(fit_eve)[i]), fill = TRUE, labels = paste0("\n(", i, "):"))
  plot(fit_eve[[i]]$GLM)
}
for (i in 1:4) {
  cat(paste0("GAM diagnostics >", names(fit_eve)[i]), fill = TRUE, labels = paste0("\n(", i, "):"))
  gam.check(fit_eve[[i]]$GAM)
}

## shannon ----
fit_shan <- lapply(results, function(x) {
  fit_glm_gam(glm_formula = Shannon ~ glacier_dist,
              gam_formula = Shannon ~ s(glacier_dist, k = 3, bs = "cr"),
              data = x,
              family = "gamma")
})
# "GAM supported: delta_AICc = 3.14"
# "GAM supported: delta_AICc = 2.94"
# "GAM supported: delta_AICc = 4.43"
# "GLM supported: delta_AICc = 1.68"
# check last one
par(mfrow = c(2, 2))
plot(fit_shan$`150 µm`$GLM) # GLM not good
for (i in 1:4) {
  cat(paste0("GAM diagnostics >", names(fit_shan)[i]), fill = TRUE, labels = paste0("\n(", i, "):"))
  gam.check(fit_shan[[i]]$GAM)
} # use GAM
for (i in 1:4) {
  cat(paste0("summary GAM >", names(fit_shan)[i]), fill = TRUE, labels = paste0("\n(", i, "):"))
  print(summary(fit_shan[[i]]$GAM))
}

## functional richness ----
fit_Frich <- lapply(results, function(x) {
  fit_glm_gam(glm_formula = Fun_richness ~ glacier_dist,
              gam_formula = Fun_richness ~ s(glacier_dist, k = 3, bs = "cr"),
              data = x,
              family = "gamma")
})
# "GAM supported: delta_AICc = 1.3"
# "GAM supported: delta_AICc = 1.75"
# "GLM supported: delta_AICc = 1.56"
# "GAM supported: delta_AICc = 0.02"
par(mfrow = c(2, 2))
for (i in 1:4) {
  cat(paste0("GLM diagnostics >", names(fit_Frich)[i]), fill = TRUE, labels = paste0("\n(", i, "):"))
  plot(fit_Frich[[i]]$GLM)
} # clear non linearity
for (i in 1:4) {
  cat(paste0("GAM diagnostics >", names(fit_Frich)[i]), fill = TRUE, labels = paste0("\n(", i, "):"))
  gam.check(fit_Frich[[i]]$GAM)
} # use GAMs for all
for (i in 1:4) {
  cat(paste0("summary GAM >", names(fit_Frich)[i]), fill = TRUE, labels = paste0("\n(", i, "):"))
  print(summary(fit_Frich[[i]]$GAM))
}

## functional evenness ----
fit_Feve <- lapply(results, function(x) {
  fit_glm_gam(glm_formula = Fun_evenness ~ glacier_dist,
              gam_formula = Fun_evenness ~ s(glacier_dist, k = 3, bs = "cr"),
              data = x,
              family = "beta")
})
# "GLM supported: delta_AICc = 0.25"
# "GLM supported: delta_AICc = 0.24"
# "GLM supported: delta_AICc = 0.24"
# "GLM supported: delta_AICc = 0.23"
par(mfrow = c(2, 2))
for (i in 1:4) {
  cat(paste0("GLM diagnostics >", names(fit_Feve)[i]), fill = TRUE, labels = paste0("\n(", i, "):"))
  plot(fit_Feve[[i]]$GLM)
}
for (i in 1:4) {
  cat(paste0("GAM diagnostics >", names(fit_Feve)[i]), fill = TRUE, labels = paste0("\n(", i, "):"))
  gam.check(fit_Feve[[i]]$GAM)
}
# GAMs are linear --> use beta regression

## rao ----
fit_rao <- lapply(results, function(x) {
  fit_glm_gam(glm_formula = rao ~ glacier_dist,
              gam_formula = rao ~ s(glacier_dist, k = 3, bs = "cr"),
              data = x,
              family = "beta")
})
# "GLM supported: delta_AICc = 3.34"
# "GLM supported: delta_AICc = 0.13"
# "GLM supported: delta_AICc = 3.08"
# "GLM supported: delta_AICc = 0.15"
par(mfrow = c(2, 2))
for (i in 1:4) {
  cat(paste0("GLM diagnostics >", names(fit_rao)[i]), fill = TRUE, labels = paste0("\n(", i, "):"))
  plot(fit_rao[[i]]$GLM)
}
for (i in 1:4) {
  cat(paste0("GAM diagnostics >", names(fit_rao)[i]), fill = TRUE, labels = paste0("\n(", i, "):"))
  gam.check(fit_rao[[i]]$GAM)
}
# where difference is negligible GAMs are linear --> use beta regression

## AICc table (supplementary table S2 in the paper) ----

AICc_table <- bind_rows(
  bind_rows(lapply(fit_rich, function(x) tibble::rownames_to_column(x$AICc, var = "model"))) %>% 
    mutate(fraction = rep(names(fit_rich), each = 2),
           response = "Richness") %>%
    select(6, 5, 1:4),
  bind_rows(lapply(fit_eve, function(x) tibble::rownames_to_column(x$AICc, var = "model"))) %>% 
    mutate(fraction = rep(names(fit_eve), each = 2),
           response = "Tax_evenness") %>%
    select(6, 5, 1:4),
  bind_rows(lapply(fit_shan, function(x) tibble::rownames_to_column(x$AICc, var = "model"))) %>% 
    mutate(fraction = rep(names(fit_shan), each = 2),
           response = "Shannon") %>%
    select(6, 5, 1:4),
  bind_rows(lapply(fit_Frich, function(x) tibble::rownames_to_column(x$AICc, var = "model"))) %>% 
    mutate(fraction = rep(names(fit_Frich), each = 2),
           response = "Fun_richness") %>%
    select(6, 5, 1:4),
  bind_rows(lapply(fit_Feve, function(x) tibble::rownames_to_column(x$AICc, var = "model"))) %>% 
    mutate(fraction = rep(names(fit_Feve), each = 2),
           response = "Fun_evenness") %>%
    select(6, 5, 1:4),
  bind_rows(lapply(fit_rao, function(x) tibble::rownames_to_column(x$AICc, var = "model"))) %>% 
    mutate(fraction = rep(names(fit_rao), each = 2),
           response = "Rao") %>%
    select(6, 5, 1:4)
) %>%
  mutate(fraction = paste0(">", fraction))

write_delim(AICc_table, "output/AICc_table.csv", delim = ";")

## plot diversity vs glacier distance (figure 5 in the paper) ----

par(mfrow = c(1, 1))

newdata <- data.frame(glacier_dist = seq(min(glacier_dist$glacier_dist), 
                                         max(glacier_dist$glacier_dist), 
                                         length.out = 100))

# col <- viridisLite::viridis(4, direction = -1) # colours

plots_rich <- lapply(1:4, function(i) {
  p <- plot_diversity(model = fit_rich[[i]]$GAM,
                      data = results[[i]],
                      newdata = newdata,
                      y = "Richness", 
                      title = paste0(">", names(results)[i]), 
                      ylab = "Taxonomic richness (S)",
                      color = col[i],
                      fill = col[i],
                      annotation_size = 2.5) +
    theme(axis.title.x = element_blank(),
          axis.title = element_text(size = 8),
          axis.text = element_text(size = 7),
          plot.title = element_text(size = 9))
  
  if (i != 1) p <- p +
      theme(axis.title.y = element_blank())
  
  p
})

plots_eve <- lapply(1:4, function(i) {
  p <- plot_diversity(model = fit_eve[[i]]$GLM,
                      data = results[[i]],
                      newdata = newdata,
                      y = "Evenness", 
                      ylab = "Taxonomic evenness (J)",
                      color = col[i],
                      fill = col[i],
                      annotation_size = 2.5) +
    theme(axis.title.x = element_blank(),
          axis.title = element_text(size = 8),
          axis.text = element_text(size = 7))
  
  if (i != 1) p <- p +
      theme(axis.title.y = element_blank())
  
  p
})

plots_shan <- lapply(1:4, function(i) {
  p <- plot_diversity(model = fit_shan[[i]]$GAM,
                      data = results[[i]],
                      newdata = newdata,
                      y = "Shannon", 
                      ylab = "Shannon index (H')",
                      color = col[i],
                      fill = col[i],
                      annotation_size = 2.5) +
    theme(axis.title.x = element_blank(),
          axis.title = element_text(size = 8),
          axis.text = element_text(size = 7))
  
  if (i != 1) p <- p +
      theme(axis.title.y = element_blank())
  
  p
})

plots_Frich <- lapply(1:4, function(i) {
  p <- plot_diversity(model = fit_Frich[[i]]$GAM,
                      data = results[[i]],
                      newdata = newdata,
                      y = "Fun_richness", 
                      ylab = "Functional richness (F<sub>Ric</sub>)",
                      color = col[i],
                      fill = col[i],
                      annotation_size = 2.5) +
    theme(axis.title.x = element_blank(),
          axis.title = element_markdown(size = 8),
          axis.text = element_text(size = 7))
  
  if (i != 1) p <- p +
      theme(axis.title.y = element_blank())
  
  p
})

plots_Feve <- lapply(1:4, function(i) {
  p <- plot_diversity(model = fit_Feve[[i]]$GLM,
                      data = results[[i]],
                      newdata = newdata,
                      y = "Fun_evenness", 
                      ylab = "Functional evenness (F<sub>Eve</sub>)",
                      color = col[i],
                      fill = col[i],
                      annotation_size = 2.5) +
    theme(axis.title.x = element_blank(),
          axis.title = element_markdown(size = 8),
          axis.text = element_text(size = 7))
  
  if (i != 1) p <- p +
      theme(axis.title.y = element_blank())
  
  p
})

plots_rao <- lapply(1:4, function(i) {
  p <- plot_diversity(model = fit_rao[[i]]$GLM,
                      data = results[[i]],
                      newdata = newdata,
                      y = "rao", 
                      xlab = "Distance (km)", 
                      ylab = "Rao",
                      color = col[i],
                      fill = col[i],
                      annotation_size = 2.5) +
    theme(axis.title = element_text(size = 8),
          axis.text = element_text(size = 7))
  
  if (i != 1) p <- p +
      theme(axis.title.y = element_blank())
  
  p
})

# combine plots
final_diversity_plot <- patchwork::wrap_plots(
  c(plots_rich, plots_eve, plots_shan, plots_Frich, plots_Feve, plots_rao),
  nrow = 6, ncol = 4) +
  patchwork::plot_annotation(theme = theme(plot.margin = margin(0, 0, 0, 0)))

# save plot
png("output/models.png", width = 18, height = 22, units = "cm", res = 300)
print(final_diversity_plot)
dev.off()

cairo_pdf("output/models.pdf", width = 18/2.54, height = 22/2.54)
print(final_diversity_plot)
dev.off()

tiff("output/models.tiff", width = 18, height = 22, units = "cm", res = 300)
print(final_diversity_plot)
dev.off()

## Functional redundancy ----

# fit models
fit_Fred <- lapply(results, function(x) {
  fit_glm_gam(glm_formula = Fun_redundancy_rel ~ glacier_dist,
              gam_formula = Fun_redundancy_rel ~ s(glacier_dist, k = 3, bs = "cr"),
              data = x,
              family = "beta")
})
# "GLM supported: delta_AICc = 2.98"
# "GLM supported: delta_AICc = 3.73"
# "GLM supported: delta_AICc = 4.02"
# "GLM supported: delta_AICc = 3.67"
par(mfrow = c(2, 2))
for (i in 1:4) {
  cat(paste0("GLM diagnostics >", names(fit_Fred)[i]), fill = TRUE, labels = paste0("\n(", i, "):"))
  plot(fit_Fred[[i]]$GLM)
}
for (i in 1:4) {
  cat(paste0("GAM diagnostics >", names(fit_Fred)[i]), fill = TRUE, labels = paste0("\n(", i, "):"))
  gam.check(fit_Fred[[i]]$GAM)
}

# plots
redundancy_plots <- lapply(1:4, function(i) {
  p <- plot_diversity(model = fit_Fred[[i]]$GLM,
                      data = results[[i]],
                      newdata = newdata,
                      y = "Fun_redundancy_rel", 
                      xlab = "Distance (km)",
                      ylab = "Relative redundancy",
                      title = paste0(">", names(results)[i]), 
                      color = col[i],
                      fill = col[i]) +
    theme(axis.title = element_markdown(size = 9),
          axis.text = element_text(size = 8),
          plot.title = element_text(hjust = 0.5, face = "bold", size = 10)) +
    ylim(0, 0.32)
  
  if (i != 1) p <- p +
      theme(axis.title.y = element_blank())
  
  p
})

# combine plots (supplementary figure S1 in the paper)
final_redundancy_plot <- patchwork::wrap_plots(redundancy_plots, nrow = 1)

# save plot
png("output/redundancy.png", width = 18, height = 6, units = "cm", res = 300)
print(final_redundancy_plot)
dev.off()

cairo_pdf("output/redundancy.pdf", width = 18/2.54, height = 6/2.54)
print(final_redundancy_plot)
dev.off()

tiff("output/redundancy.tiff", width = 18, height = 6, units = "cm", res = 300)
print(final_redundancy_plot)
dev.off()
