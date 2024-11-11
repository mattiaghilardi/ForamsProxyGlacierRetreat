## Create map of study area with GEBCO bathymetry

# set directory with bathymetry ----

options(ggOceanMaps.userpath = "data/GEBCO/gebco_2024_n85.0_s72.0_w5.0_e35.0.nc")

## map of svalbard ----

# create arrow for WSC
# need to use latitude for x and longitude for y to get a correct spline and then invert
wsc <- spline(x = c(76, 76.4, 77, 77.5, 78, 78.5, 79, 79.5, 80, 80.5),
              y = c(13.6, 13.8, 12.2, 10.8, 9.7, 8.9, 8.3, 8, 8.5, 10)) %>%  
  rlang::set_names("lat", "long")

# plot
p1 <- ggOceanMaps::basemap(limits = c(9, 26, 76, 81),
                           bathy.style = "rub",
                           shapefiles = "Svalbard",
                           legends = FALSE,
                           land.col = "grey60",
                           land.border.col = "grey60") +
  theme(axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        panel.border = element_rect(linewidth = 0.8),
        plot.margin = margin(0, 0, -0.3, -0.2, "lines")) +
  ggspatial::geom_spatial_rect(aes(xmin = 11.6, xmax = 12.7, 
                                   ymin = 78.83, ymax = 79.03),
                               color = "black", fill = NA, linewidth = 0.4) +
  ggspatial::geom_spatial_path(aes(x = wsc$long, y = wsc$lat),
                               arrow = arrow(angle = 25, 
                                             length = unit(0.15, "inches")),
                               color = "red", linewidth = 1) +
  ggspatial::geom_spatial_text(aes(x = 11, y = 76.5), 
                               label = "wsc", color = "red", size = 3.5)

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
  long = c(12.055, 12.55, 12.61, 12.62, 12.6),
  angle = c(0, 0, 0, 0, -30)
  )

# load shapefile of svalbard glaciers
# this is the file downloaded by ggOceanMaps
load("data/ggOceanMaps/svalbard_glacier.rda")

# extract geometry of continental glaciers to fill with pattern
continental_glaciers <- svalbard_glacier[-885,] %>% 
  sf::st_crop(xmin = 426511, xmax = 451352, 
              ymin = 8751930, ymax = 8775425)

# plot
p2 <- ggOceanMaps::basemap(limits = c(11.6, 12.75, 78.83, 79.03),
                           bathy.style = "rub", 
                           shapefiles = "Svalbard",
                           glaciers = TRUE,
                           gla.col = "grey95",
                           lon.interval = 0.2,
                           lat.interval = 0.05) +
  labs(y = "Latitude",
       x = "Longitude") +
  theme(axis.title = element_text(size = 10),
        axis.text = element_text(size = 8),
        legend.title = element_text(size = 10),
        legend.text = element_text(size = 8),
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
  ggspatial::geom_spatial_point(aes(x = 11.922222, y = 78.925), shape = 15) +
  ggspatial::geom_spatial_text(aes(x = 11.922222, y = 78.925),
                               label = "Ny-Ålesund", 
                               size = 3.5, hjust = 1.1) +
  ggspatial::geom_spatial_text(data = tidewater_glaciers,
                               aes(x = long, y = lat, label = glacier, angle = angle),
                               size = 2.5, hjust = 0) +
  ggspatial::annotation_north_arrow(location = "tl", 
                                    which_north = "true", 
                                    height = unit(0.6, "cm"),
                                    width = unit(0.6, "cm"), 
                                    pad_x = unit(0.4, "cm"), 
                                    style = ggspatial::north_arrow_orienteering(fill = c("black", "black"), 
                                                                                text_size = 6)) +
  ggspatial::annotation_scale(location = "tl",
                              height = unit(0.2, "cm"),
                              style = "ticks",
                              width_hint = 0.1,
                              pad_y = unit(1, "cm"))

## final plot ----

# combine plots
final_map <- p2 + 
  inset_element(p1, left = 1e-3, bottom = 1e-3, right = 0.31, top = 0.43) + 
  plot_annotation(theme = theme(plot.margin = margin(0, 0, 0, 0)))

# save plot
png("output/map.png", width = 16, height = 12, units = "cm", res = 300)
print(final_map)
dev.off()

cairo_pdf("output/map.pdf", width = 16/2.54, height = 12/2.54)
print(final_map)
dev.off()

tiff("output/map.tiff", width = 16, height = 12, units = "cm", res = 300)
print(final_map)
dev.off()

## clean environment ----
# keep coord
rm(list = ls()[ls() != "coord"])
