## Create map of study area with GEBCO bathymetry

# set directory with bathymetry ----

options(ggOceanMaps.userpath = "data/GEBCO/gebco_2024_n85.0_s72.0_w5.0_e35.0.nc")

## map of arctic ocean

p1 <- ggOceanMaps::basemap(limits = 65, 
                           shapefiles = "Arctic",
                           land.col = "grey60",
                           land.border.col = "grey60") + 
  ggspatial::geom_spatial_rect(aes(xmin = 6, xmax = 32, 
                                   ymin = 75.5, ymax = 81.5),
                               color = "black", 
                               fill = NA, 
                               linewidth = 0.5) +
  ggspatial::geom_spatial_text(aes(x = 0, y = 90),
                               label = "Arctic Ocean",
                               size = 2) +
  ggspatial::geom_spatial_text(aes(x = x, y = y,
                                   label = paste0(x, "\u00B0")),
                               data = data.frame(x = seq(0, 315, by = 45),
                                                 y = 62),
                               size = 2) +
  ggspatial::geom_spatial_text(aes(x = x, y = y,
                                   label = paste0(y, "\u00B0")),
                               data = data.frame(x = 180,
                                                 y = seq(70, 85, by = 5)),
                               size = 2) +
  coord_sf(expand = TRUE)


## map of svalbard ----

# create arrow for currents
# need to use latitude for x and longitude for y to get a correct spline and then invert
wsc <- spline(x = c(75.5, 75.8, 76, 76.4, 77, 77.5, 78, 78.5, 79, 79.5, 80, 80.5),
              y = c(13.6, 13.5, 13.6, 13.8, 12.2, 10.8, 9.7, 8.9, 8.3, 8, 8.5, 10)) %>%  
  rlang::set_names("lat", "long")

esc <- spline(x = c(79.5, 79, 78.5, 78, 77, 76.8),
              y = c(28, 25.5, 25, 25.5, 24, 23)) %>%  
  rlang::set_names("lat", "long")

spc <- spline(x = c(76.2, 76.4, 77, 77.5, 78, 78.5, 79, 80),
              y = c(17.8, 16, 13.5, 12.3, 11.2, 10.3, 9.7, 10)) %>%  
  rlang::set_names("lat", "long")

# plot
p2 <- ggOceanMaps::basemap(limits = c(9, 26, 75.5, 81.3),
                           bathymetry = FALSE,
                           shapefiles = "Svalbard",
                           land.col = "grey60",
                           land.border.col = "grey60") +
  labs(y = "Latitude",
       x = "Longitude") +
  theme(axis.title = element_text(size = 10),
        axis.text = element_text(size = 8),
        panel.border = element_rect(linewidth = 0.8)) +
  ggspatial::geom_spatial_rect(aes(xmin = 11.6, xmax = 12.76, 
                                   ymin = 78.83, ymax = 79.07),
                               color = "black", 
                               fill = NA, 
                               linewidth = 0.5) +
  ggspatial::geom_spatial_path(aes(x = wsc$long, y = wsc$lat),
                               arrow = arrow(angle = 30, 
                                             length = unit(0.1, "inches"),
                                             type = "open"),
                               color = "red", 
                               linewidth = 1) +
  ggspatial::geom_spatial_text(aes(x = 11, y = 76.5), 
                               label = "WSC", 
                               color = "red", 
                               size = 3) +
  ggspatial::geom_spatial_path(aes(x = esc$long, y = esc$lat),
                               arrow = arrow(angle = 30, 
                                             length = unit(0.1, "inches"),
                                             ends = "first",
                                             type = "open"),
                               color = "mediumblue", 
                               linewidth = 1) +
  ggspatial::geom_spatial_text(aes(x = 27, y = 78.4), 
                               label = "ESC", 
                               color = "mediumblue", 
                               size = 3) +
  ggspatial::geom_spatial_path(aes(x = spc$long, y = spc$lat),
                               arrow = arrow(angle = 30, 
                                             length = unit(0.1, "inches"),
                                             type = "open"),
                               color = "mediumblue", 
                               linewidth = 1) +
  ggspatial::geom_spatial_text(aes(x = 16.5, y = 76), 
                               label = "SPC", 
                               color = "mediumblue", 
                               size = 3) +
  ggspatial::geom_spatial_text(aes(x = 18, y = 81.1), 
                               label = "Svalbard", 
                               size = 3.5) +
  ggspatial::annotation_north_arrow(location = "br", 
                                    which_north = "true", 
                                    height = unit(0.4, "cm"),
                                    width = unit(0.4, "cm"),
                                    pad_x = unit(0.4, "cm"),
                                    pad_y = unit(0.6, "cm"), 
                                    style = ggspatial::north_arrow_orienteering(fill = c("black", "black"), 
                                                                                text_size = 5)) +
  ggspatial::annotation_scale(location = "br",
                              height = unit(0.2, "cm"),
                              style = "ticks",
                              width_hint = 0.2)

## map of study area ----

# load stations coordinates
coord <- readr::read_delim("data/coordinate_king18.csv", delim = ";")

# convert coordinates to decimal degrees
coord$lat <- measurements::conv_unit(coord$lat, from = "deg_dec_min", to = "dec_deg") %>% 
  as.numeric()
coord$long <- measurements::conv_unit(coord$long, from = "deg_dec_min", to = "dec_deg") %>% 
  as.numeric()

# create data frame with tidewater glaciers coordinates to add names to map
tidewater_glaciers <- data.frame(
  glacier = c("Blomstrandbreen", "Conwaybreen", "Kongsbreen", "Kronebreen", "Kongsvegen"),
  lat = c(79.025, 78.99, 78.93, 78.875, 78.855),
  long = c(12.18, 12.55, 12.61, 12.62, 12.6),
  angle = c(65, 0, 0, 0, -30)
)

# load shapefile of svalbard glaciers
# this is the file downloaded by ggOceanMaps
load("data/ggOceanMaps/svalbard_glacier.rda")

# extract geometry of continental glaciers to fill with pattern
continental_glaciers <- svalbard_glacier[-885,] %>% 
  sf::st_crop(xmin = 426511, xmax = 451352, 
              ymin = 8751930, ymax = 8775425)

# plot
p3 <- ggOceanMaps::basemap(limits = c(11.6, 12.76, 78.83, 79.07),
                           bathy.style = "rub", 
                           shapefiles = "Svalbard",
                           glaciers = TRUE,
                           gla.col = "grey92",
                           lon.interval = 0.2,
                           lat.interval = 0.05) +
  labs(y = "Latitude",
       x = "Longitude") +
  theme(axis.title = element_text(size = 10),
        axis.text = element_text(size = 8),
        legend.title = element_text(size = 10),
        legend.text = element_text(size = 8),
        legend.position = "inside",
        legend.position.inside = c(0, 0), 
        legend.justification.inside = c(0, 0),
        legend.background = element_rect(color = "black"),
        legend.margin = margin(t = 0.2, r = 0.2, b = 0.2, l = 0.2, unit = "cm"),
        panel.border = element_rect(linewidth = 0.8)) + 
  ggpattern::geom_sf_pattern(data = continental_glaciers, 
                             pattern = "circle", 
                             pattern_spacing = 0.01,
                             pattern_color = "grey40",
                             pattern_fill = "grey40") +
  ggspatial::geom_spatial_point(data = coord[1:9,], 
                                aes(x = long, y = lat)) +
  ggspatial::geom_spatial_text_repel(data = coord[1:9,],
                                     aes(x = long, y = lat,
                                         label = gsub("s", "S", station)),
                                     size = 3.5, 
                                     seed = 999) +
  ggspatial::geom_spatial_point(aes(x = 11.922222, y = 78.925), 
                                shape = 15) +
  ggspatial::geom_spatial_text(aes(x = 11.922222, y = 78.932),
                               label = "Ny-Ålesund", 
                               size = 3.5, 
                               hjust = 0.85) +
  ggspatial::geom_spatial_text(data = tidewater_glaciers,
                               aes(x = long, y = lat, label = glacier, angle = angle),
                               size = 2.5, hjust = 0) +
  ggspatial::annotation_north_arrow(location = "tr", 
                                    which_north = "true", 
                                    height = unit(0.6, "cm"),
                                    width = unit(0.6, "cm"),
                                    pad_x = unit(0.5, "cm"), 
                                    style = ggspatial::north_arrow_orienteering(fill = c("black", "black"), 
                                                                                text_size = 6)) +
  ggspatial::annotation_scale(location = "tr",
                              height = unit(0.2, "cm"),
                              style = "ticks",
                              width_hint = 0.15,
                              pad_y = unit(1, "cm"))

## final plot (figure 1 in the paper) ----

# combine plots
final_map <- p1 + p2 + p3 +
  plot_layout(
    design = "
    AAACCCCCCC
    AAACCCCCCC
    AAACCCCCCC
    BBBCCCCCCC
    BBBCCCCCCC
    BBBCCCCCCC
    BBBCCCCCCC
    ") +
  plot_annotation(tag_levels = "A") &
  theme(plot.tag.position = c(0, 1),
        plot.tag = element_text(face = "bold", hjust = 0, vjust = 0))

# save plot
png("output/map.png", width = 18, height = 12, units = "cm", res = 300)
print(final_map)
dev.off()

cairo_pdf("output/map.pdf", width = 18/2.54, height = 12/2.54)
print(final_map)
dev.off()

tiff("output/map.tiff", width = 18, height = 12, units = "cm", res = 300)
print(final_map)
dev.off()

## clean environment ----

# keep coord
rm(list = ls()[ls() != "coord"])
