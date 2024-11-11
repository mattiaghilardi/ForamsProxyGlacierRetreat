## Compute taxonomic and functional diversity for different size fractions
## of live benthic foraminifera assemblages in the Kongsfjorden (Svalbard)

# ## load data ----
# 
# comm_king18 <- readr::read_delim("data/foram_data_king18.csv", delim = ";")
# traits <- readr::read_delim("data/species_traits.csv", delim = ";")
# glacier_dist <- readr::read_delim("derived_data/glacier_dist.csv", delim = ";")

## data preparation ----

# create a list of four matrices
matrix_list <- lapply(fraction, function(i)
  comm_king18 %>% 
    # species cumulative densities
    combine_communities() %>%
    # rearrange the dataset to have a column for each species
    pivot_wider(names_from = "species", values_from = "raw_density") %>% 
    # keep only the i fraction
    filter(fraction == i) %>% 
    select(-fraction) %>%
    column_to_rownames(var = "station")
) %>% 
  rlang::set_names(fraction)

# rearrange the trait dataset by species and use species names as row names
traits <- traits %>%
  arrange(species) %>% # alphabetic order
  column_to_rownames(var = "species")

# potential number of functional entities (i.e. total potential trait combinations)
2^4*4*3 # 192
# actual number of functional entities
nrow(unique(traits)) # 27

## taxonomic diversity ----

taxo_diversity <- lapply(matrix_list, taxo_div)

## functional diversity ----

# Use trait probability density functions (TPD) to calculate functional diversity metrics:
# richness, evenness, and Rao
# First get TPD functions for each species in the dataset (TPDs or TPDsMean), 
# then use these to get the TPD functions of the communities (TPDc)

# Since we have categorical and binary traits, 
# first we need to convert them to a distance matrix (Gower distance),
# then transform them with a PCoA and 
# use the species position on the resultant axes as new "traits".

# De Bello et al. 2020 explained that the Gower dissimilarity does not 
# give the same weight to each trait,
# and created a new R function "gawdis" that actually does this.
# Try both and check differences.

# Check trait weights given by the two approaches,
# and the correlation between the two dissimilarity matrices.
# Create function to check these correlations,
# returns a list with:
# 1- gowdis dissimilarity matrix
# 2- gawdis dissimilarity matrix
# 3- tibble with correlations between gowdis or gawdis and the dissimilarity matrices of individual traits
# 4- correlation between gowdis and gawdis
check_dist_corr <- function(traits) {
  # Dissimilarity matrices with whole dataset
  # Gower with package FD
  gowdis <- FD::gowdis(traits)
  # Gawdis with the respective package
  gawdis <- gawdis::gawdis(traits)
  
  # Dissimilarity matrices of single traits
  # Function dist() for binary traits and gowdis() for categorical traits
  num <- names(dplyr::select(traits, where(is.numeric)))
  chr <- names(dplyr::select(traits, where(is.character)))
  dist_list <- list()
  for (i in num) {
    dist <- dist(traits[,i])
    dist_list[[i]] <- dist
  }
  for (i in chr) {
    dist <- FD::gowdis(traits[,i, drop = FALSE])
    dist_list[[i]] <- dist
  }
  
  # Reorder list
  dist_list <- dist_list[names(traits)]
  
  # Correlations
  gowdis_cor <- c()
  for (i in dist_list) {
    cor <- cor(gowdis, i)
    gowdis_cor <- c(gowdis_cor, cor)
  }
  
  # Return a list
  list(
    gowdis = gowdis,
    gawdis = gawdis,
    # tibble with trait weight for gowdis and gawdis
    correls = tibble(trait = names(traits), 
                     gowdis = gowdis_cor,
                     gawdis = attr(gawdis, "correls")),
    # correlation between the two matrices
    cor = cor(gowdis, gawdis)
  )
}

dist_check <- check_dist_corr(traits)
dist_check$correls
# Correlations between gowdis and individual dissimilarity matrices vary between 0.371 and 0.618
# gawdis correlate equally to the individual dissimilarity matrices (r=0.461)
dist_check$cor
# The two matrices don't differ much (r=0.97)

# compute functional diversity for each fraction using gawdis
func_diversity <- lapply(matrix_list, function(x) 
  func_div_TPD(x, traits, dist = "gawdis", k = 3, # k is the number of retained dimensions
               REND = TRUE, rao = TRUE, redun = TRUE, 
               dissim = FALSE, uniq = FALSE, regional = FALSE))

# check PCOA
# barplot of the eigenvalues
barplot(func_diversity[[1]]$PCOA$eig)

# variance explained by the first 3 axes
sum(func_diversity[[1]]$PCOA$eig[1:3]) / 
  sum(func_diversity[[1]]$PCOA$eig[func_diversity[[1]]$PCOA$eig > 0]) # 0.78

## create results table ----

# create data frame combining all diversity metrics
results <- lapply(fraction, function(i) {
  func_diversity[[i]]$diversity <- cbind(func_diversity[[i]]$diversity, 
                                         rao = func_diversity[[i]]$rao$alpha_rao)
  left_join(taxo_diversity[[i]], 
            func_diversity[[i]]$diversity, 
            by = c("Station" = "Comm"))
}) %>% 
  rlang::set_names(fraction)

saveRDS(results, "derived_data/diversity_results.rds")

results_table <- bind_rows(results, .id = "Fraction") %>%
  left_join(glacier_dist[, c(1, 4)], by = c("Station" = "station")) %>% 
  mutate(Fraction = factor(Fraction, 
                           levels = paste(c(63, 100, 125, 150), "µm"),
                           labels = paste0(">", c(63, 100, 125, 150), " µm")),
         Station = gsub("s", "S", Station)) %>%
  select(-c(8:10)) %>%
  arrange(Fraction, glacier_dist) %>% 
  select(-glacier_dist) %>%
  pivot_longer(cols = 3:8, names_to = "Diversity_metrics") %>% 
  pivot_wider(names_from = Station, values_from = value) %>%
  mutate(Diversity_metrics = recode(Diversity_metrics, "rao" = "Rao"))

write_delim(results_table, "output/diversity_table.csv", delim = ";")

## correlations among diversity metrics ----

correls <- lapply(fraction, function(x) {
  results[[x]] %>% 
    select(c(2:6, 10)) %>% 
    rename("S" = "Richness",
           "J" = "Evenness",
           "H'" = "Shannon",
           "FRic" = "Fun_richness",
           "FEve" = "Fun_evenness",
           "Rao" = "rao") %>% 
    cor()
}) %>% 
  rlang::set_names(fraction)

for (i in 1:4) {
  png(paste0("output/correlations_", fraction[[i]], ".png"), 
      units = "cm", width = 14, height = 13, res = 300, type = "cairo")
  corrplot::corrplot.mixed(correls[[i]], tl.col = "black", tl.cex = 1)
  title(paste0(">", fraction[[i]]), line = -1, outer = TRUE)
  dev.off()
}

## correlations among size fractions ----

# create a vector containing the diversity metrics names
metrics <- names(results[[1]])[c(2:6, 10)]

# create a list of tibbles with results by metric
results_by_metric <- lapply(metrics, function(x) {
  tibble(station = c(paste0("st", 1:9)),
         '>63 µm' = results[[1]][, x, drop = TRUE],
         '>100 µm' = results[[2]][, x, drop = TRUE],
         '>125 µm' = results[[3]][, x, drop = TRUE],
         '>150 µm' = results[[4]][, x, drop = TRUE])
}) %>% 
  rlang::set_names(metrics)

# plot correlations

cor_custom <- function(data, mapping, ...) {
  
  xdata <- rlang::eval_tidy(mapping$x, data = data)
  ydata <- rlang::eval_tidy(mapping$y, data = data)
  corObj <- stats::cor.test(xdata, ydata, method = "pearson")
  cor_est <- as.numeric(corObj$estimate)
  cor_txt <- formatC(cor_est, digits = 3, format = "f")
  stars <- signif_stars(corObj$p.value)
  cor_txt <- paste0(cor_txt, stars)
  
  ggally_text(label = cor_txt, ...) +
    theme_bw() +
    theme(panel.grid = element_blank())
}

diag_custom <- function(data, mapping, ...) {
  
  ggally_text(label = as_label(mapping$x), ...) +
    theme_bw() +
    theme(panel.grid = element_blank())
}

smooth_custom <- function(data, mapping, method = "lm", formula = y ~ x, se = TRUE, ...) {
  
  ggplot(data = data, mapping = mapping, ...) +
    geom_point(...) +
    geom_smooth(method = method, formula = formula, se = se, ...) +
    theme_bw() +
    theme(panel.grid = element_blank())
}

# col <- viridisLite::viridis(4, direction = -1) # colours

metric_cor_plot <- mapply(function(x, y) {
  p <- ggpairs(
    x[, 2:5], 
    title = y,
    upper = list(continuous = wrap(cor_custom, size = 2.7, color = "black")),
    lower = list(continuous = wrap(smooth_custom, color = "black", size = 0.8, linewidth = 0.8)),
    diag = list(continuous = wrap(diag_custom, size = 2.7, fontface = "bold")),
    axisLabels = "none"
  ) + 
    theme(strip.background = element_blank(),
          strip.text = element_blank(),
          plot.title = element_markdown(face = "bold", hjust = 0.5, size = 9))
  
  for(i in 1:4) {
    p[i, i] <- p[i, i] + 
      theme(panel.border = element_rect(linewidth = 1,
                                        color = col[i],
                                        fill = alpha(col[i], 0.2)))
  }
  
  ggmatrix_gtable(p)
}, 
x = results_by_metric, 
y = c("Taxonomic richness (S)", 
      "Taxonomic evenness (J)",
      "Shannon index (H')", 
      "Functional richness (F<sub>Ric</sub>)",
      "Functional evenness (F<sub>Eve</sub>)", 
      "Rao"))

# combine plots
final_metric_cor_plot <- patchwork::wrap_plots(metric_cor_plot, ncol = 3, nrow = 2) + 
  patchwork::plot_annotation(theme = theme(plot.margin = margin(0, 0, 0, 0)))

# save plot
png("output/corr_matrix.png", width = 18, height = 13, units = "cm", res = 300)
print(final_metric_cor_plot)
dev.off()

cairo_pdf("output/corr_matrix.pdf", width = 18/2.54, height = 13/2.54)
print(final_metric_cor_plot)
dev.off()

tiff("output/corr_matrix.tiff", width = 18, height = 13, units = "cm", res = 300)
print(final_metric_cor_plot)
dev.off()

## clean environment ----
# keep glacier_dist, col and results
rm(list = ls()[!ls() %in% c("glacier_dist", "col", "results")])
