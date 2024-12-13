## Calculate the distance of each station from the glacier front

# # load stations coordinates
# coord <- readr::read_delim("data/coordinate_king18.csv", delim = ";")
# 
# ## measure distance between station and glacier front ----
# 
# # convert coordinates to decimal degrees
# coord$lat <- measurements::conv_unit(coord$lat, from = "deg_dec_min", to = "dec_deg") %>% 
#   as.numeric()
# coord$long <- measurements::conv_unit(coord$long, from = "deg_dec_min", to = "dec_deg") %>% 
#   as.numeric()

# get distance between station and glacier front in Km
glacier_dist <- c()
for (i in 1:9) {
  glacier_dist[i] <- round(geosphere::distm(c(coord$long[10], coord$lat[10]), 
                                            c(coord$long[i], coord$lat[i]), 
                                            fun = geosphere::distGeo)/1000, 2)
}

# add distance to the dataset
glacier_dist <- cbind(coord[1:9,], glacier_dist)

# save file
write_delim(glacier_dist, "derived_data/glacier_dist.csv", delim = ";" )

## clean environment ----

rm(coord, i)
