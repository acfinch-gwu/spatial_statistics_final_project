library(sf)
library(tmap)
library(tigris)

library(terra)  # For raster operations
library(dplyr)

# Set tmap to interactive mode
tmap_mode("view")

# Load FEMA flood hazard shapefile
flood_data <- st_read("./data/110001_20241031/S_FLD_HAZ_AR.shp") %>%
  st_make_valid()

unique(flood_data$FLD_ZONE)
table(flood_data$FLD_ZONE, useNA = "ifany")

# Filter for only A and AE zones
flood_A_AE <- flood_data %>%
  filter(FLD_ZONE %in% c("A", "AE"))

# Plot interactively
tmap_mode("view")

tm_shape(flood_A_AE) +
  tm_polygons("FLD_ZONE", palette = "Set2", alpha = 0.5) +
  tm_layout(title = "FEMA Flood Zones: A and AE Only")

# Set tigris to use cache
options(tigris_use_cache = TRUE)

#  Load DC census tracts
tracts <- tracts(state = "DC", cb = TRUE, year = 2020)

#  Match CRS between tracts and flood zones
tracts <- st_transform(tracts, st_crs(flood_A_AE))

#  Plot
tmap_mode("view")  # interactive map

tm_shape(tracts) +
  tm_borders(col = "gray50", lwd = 1) +
tm_shape(flood_A_AE) +
  tm_polygons("FLD_ZONE", palette = "Set2", alpha = 0.6, border.col = "black", border.alpha = 0.3) +
tm_layout(title = "FEMA Flood Zones A & AE Overlaid on DC Census Tracts")

results_summary <- results %>%
  group_by(GEOID, FLD_ZONE) %>%
  summarize(
    intersection_area_m2 = sum(intersection_area_m2, na.rm = TRUE),
    tract_area_m2 = first(tract_area_m2),  # same for all rows with same GEOID
    .groups = "drop"
  ) %>%
  mutate(
    percent_in_flood_zone = 100 * intersection_area_m2 / tract_area_m2
  )


results_total <- results_summary %>%
  group_by(GEOID) %>%
  summarize(
    total_flood_area_m2 = sum(intersection_area_m2),
    tract_area_m2 = first(tract_area_m2),
    percent_flooded = 100 * total_flood_area_m2 / tract_area_m2,
    .groups = "drop"
  )

##############

