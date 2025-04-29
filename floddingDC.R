library(sf)
library(tmap)
library(tigris)

library(terra)  # For raster operations
library(dplyr)

# # Set tmap to interactive mode
# tmap_mode("view")

# Load FEMA flood hazard shapefile
flood_data <- st_read("./shapefiles/md_nfhl.shp")

nrow(flood_data) / 10

# st_is_valid(flood_data)
flood_data = st_make_valid(flood_data[1:600, ])

unique(flood_data$FLD_ZON)
table(flood_data$FLD_ZON, useNA = "ifany")

# Filter for only A and AE zones
flood_zones <- flood_data %>%
  filter(FLD_ZON %in% c("A", "AE", "AH", "AO", "VE"))

# library(sf)
# library(parallel)
# library(dplyr)
# 
# # 1) Read the full flood hazard shapefile
# flood_data <- st_read("./shapefiles/md_nfhl.shp")
# 
# # 2) Decide how many cores to use (leave 1 core free for OS/other tasks)
# n_cores <- detectCores() - 1
# 
# # 3) Split the sf object into 'n_cores' chunks by row
# chunk_ids <- split(1:nrow(flood_data),
#                    cut(1:nrow(flood_data), breaks = n_cores, labels = FALSE))
# flood_chunks <- lapply(chunk_ids, function(idxs) flood_data[idxs, ])
# 
# # 4) Launch a cluster, load sf on each worker, and run st_make_valid in parallel
# cl <- makeCluster(n_cores)
# clusterEvalQ(cl, library(sf))    # load sf on each worker
# valid_chunks <- parLapply(cl, flood_chunks, st_make_valid)
# stopCluster(cl)
# 
# # 5) Re‐combine the now‐valid pieces back into one sf
# flood_data_valid <- do.call(rbind, valid_chunks)
# 
# # 6) Finally, filter for your zones of interest
# flood_zones <- flood_data_valid %>%
#   filter(FLD_ZON %in% c("A", "AE", "AH", "AO", "VE"))

# # Plot interactively
# tmap_mode("view")
# 
# tm_shape(flood_zones[1:600, ]) +
#   tm_polygons("FLD_ZON", palette = "Set2", alpha = 0.5) +
#   tm_layout(title = "FEMA Flood Zones: A and AE Only")

# Set tigris to use cache
options(tigris_use_cache = TRUE)

#  Load DC census tracts
tracts <- tracts(state = "MD", cb = TRUE, year = 2022)

#  Match CRS between tracts and flood zones
tracts <- st_transform(tracts, st_crs(flood_A_AE))
tracts$tract_area_m2 <- st_area(tracts)
flood_intersection <- st_intersection(tracts, flood_zones)


flood_intersection$intersection_area_m2 <- st_area(flood_intersection)

flood_summary <- flood_intersection %>%
  st_drop_geometry() %>%
  group_by(GEOID) %>%
  summarize(total_flood_area_m2 = sum(as.numeric(intersection_area_m2), na.rm = TRUE), .groups = "drop")

tracts <- tracts %>%
  left_join(flood_summary, by = "GEOID") %>%
  mutate(
    total_flood_area_m2 = replace_na(total_flood_area_m2, 0),
    percent_flooded = 100 * total_flood_area_m2 / as.numeric(tract_area_m2),
    in_flood_zone = as.integer(total_flood_area_m2 > 0)
  )

# #  Plot
# tmap_mode("view")  # interactive map
# 
# tm_shape(tracts) +
#   tm_borders(col = "gray50", lwd = 1) +
# tm_shape(flood_zones) +
#   tm_polygons("FLD_ZONE", palette = "Set2", alpha = 0.6, border.col = "black", border.alpha = 0.3) +
# tm_layout(title = "FEMA Flood Zones A & AE Overlaid on DC Census Tracts")

# results_summary <- results %>%
#   group_by(GEOID, FLD_ZONE) %>%
#   summarize(
#     intersection_area_m2 = sum(intersection_area_m2, na.rm = TRUE),
#     tract_area_m2 = first(tract_area_m2),  # same for all rows with same GEOID
#     .groups = "drop"
#   ) %>%
#   mutate(
#     percent_in_flood_zone = 100 * intersection_area_m2 / tract_area_m2
#   )
# 
# 
# results_total <- results_summary %>%
#   group_by(GEOID) %>%
#   summarize(
#     total_flood_area_m2 = sum(intersection_area_m2),
#     tract_area_m2 = first(tract_area_m2),
#     percent_flooded = 100 * total_flood_area_m2 / tract_area_m2,
#     .groups = "drop"
#   )

##############

