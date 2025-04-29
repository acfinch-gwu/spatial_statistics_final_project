library(sf)
library(dplyr)
library(tigris)
library(data.table)
# Set up
options(tigris_use_cache = TRUE)
# Plane 
sf::sf_use_s2(FALSE)

# Load FEMA flood hazard shapefiles (already filtered for SFHA_TF == "T")
dc_flood = st_read("shapefiles/dc_nfhl.shp") %>% filter(SFHA_TF == "T")
md_flood = st_read("shapefiles/md_nfhl.shp") %>% filter(SFHA_TF == "T")
va_flood = st_read("shapefiles/va_nfhl.shp") %>% filter(SFHA_TF == "T")

# Merge all flood hazard areas
all_flood = bind_rows(dc_flood, md_flood, va_flood)

# Load Census Tracts for DC, MD, VA
dc_tracts = tracts(state = "DC", year = 2022, class = "sf")
md_tracts = tracts(state = "MD", year = 2022, class = "sf")
va_tracts = tracts(state = "VA", year = 2022, class = "sf")

# Merge all tracts
all_tracts = bind_rows(dc_tracts, md_tracts, va_tracts)

# Ensure CRS match
all_tracts = st_transform(all_tracts, st_crs(all_flood))

# Project to plane
all_tracts = all_tracts %>% st_transform(26918)
all_flood  = all_flood  %>% st_transform(26918)

# Spatial intersection: Tracts and Flood Zones
tracts_candidate = st_filter(all_tracts, all_flood)
intersections = st_intersection(tracts_candidate, all_flood)
# intersections = st_intersection(all_tracts, all_flood)

# Calculate areas
intersections = intersections %>%
  mutate(
    intersection_area_m2 = st_area(.),
    tract_area_m2 = st_area(all_tracts[match(GEOID, all_tracts$GEOID), ])
  )

# Summarize percent flooded for each Tract
results_summary = intersections %>%
  group_by(GEOID) %>%
  summarize(
    total_flood_area_m2 = sum(as.numeric(intersection_area_m2), na.rm = TRUE),
    tract_area_m2 = first(as.numeric(tract_area_m2)),
    .groups = "drop"
  ) %>%
  mutate(
    percent_in_flood_zone = 100 * total_flood_area_m2 / tract_area_m2,
    flood_risk = ifelse(percent_in_flood_zone > 0, 1, 0)
  )

# Check
head(results_summary)

# Save results
#write.csv(results_summary, "data/dmv_flood_risk_percent.csv", row.names = FALSE)
df <- results_summary %>% st_drop_geometry()
fwrite(df, "data/dmv_flood_risk_percent.csv")

# # Check
# head(results_summary)
# 
# # Save results
# write.csv(results_summary, "data/dmv_flood_risk_percent.csv", row.names = FALSE)
