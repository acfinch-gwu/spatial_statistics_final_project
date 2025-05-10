# ----------------------------------------------------------------
# This is a single script compiling all the code for the project.
# The code was spread across 1 script and 6 markdown files
# For a better (and more easily executable) form of this code, 
# see the Github linked below:
# https://github.com/acfinch-gwu/spatial_statistics_final_project
# ----------------------------------------------------------------

require(sf)
require(ggplot2)
require(tidyverse)
require(broom)
require(fields)
require(geoR)
require(tmap)
require(spdep)
require(sphet)
require(sp)
require(INLA)
require(gstat)
require(stringr)

# LOAD AND INSPECT DATA
# _____________________

data_dir = "./data/"

## Asthma Equity Data

files_list = list.files("./data/asthma_equity_data", pattern = ".csv", full.names = T)
dataframe_list = lapply(files_list, read.csv)
asthma_data = dataframe_list[[1]]
for (i in 2:(length(dataframe_list))) {
  asthma_data = rbind(asthma_data, dataframe_list[[i]])
}

colnames(asthma_data)
drop_indices = c(18:22, 32, 33, 34, 37, 38)
asthma_data = asthma_data[, -drop_indices]
colnames(asthma_data) <- c(
  "fips",
  "location",
  "unw_cent_long",
  "unw_cent_lat",
  "est_population",
  "non_his",
  "white",
  "black",
  "amer_indian",
  "asian",
  "hawaiian",
  "other",
  "two_or_more",
  "hispanic",
  "educated",
  "med_income",
  "below_pov",
  "high_school",
  "college",
  "labor_participation",
  "non_white",
  "redlining",
  "asthma_prev",
  "smoking_prev",
  "obesity_prev",
  "copd_prev",
  "particulate_matter",
  "housing_stress"
)

asthma = as_tibble(asthma_data)
write_csv(asthma, "asthma_equity_data.csv")

## Places Data

places_data = read.csv(str_c(data_dir, "dc_md_va_places_census_tract_data_2022.csv"))
colnames(places_data)
places_features = c(
  "StateAbbr",
  "StateDesc",
  "CountyName",
  "TractFIPS",
  "TotalPopulation",
  "ACCESS2_CrudePrev",
  "BINGE_CrudePrev",
  "BPHIGH_CrudePrev",
  "CANCER_CrudePrev",
  "CASTHMA_CrudePrev",
  "CHD_CrudePrev",
  "CHECKUP_CrudePrev",
  "COPD_CrudePrev",
  "CSMOKING_CrudePrev",
  "DIABETES_CrudePrev",
  "HIGHCHOL_CrudePrev",
  "LPA_CrudePrev",
  "OBESITY_CrudePrev",
  "SLEEP_CrudePrev",
  "STROKE_CrudePrev",
  "Geolocation"
)
places_data = select(places_data, places_features)
colnames(places_data) <- c(
  "state_abbrev",
  "state_desc", 
  "county_name",
  "tract_fips",
  "total_population",
  "lack_insurence_prev",
  "binge_drinking_prev",
  "high_bp_prev",
  "cancer_prev",
  "adult_asthma_prev",
  "heart_disease_prev",
  "routine_physical_prev",
  "copd_prev",
  "smoking_prev",
  "diabetes_prev",
  "high_chol_prev",
  "physical_activity_prev",
  "adult_obesity_prev",
  "poor_sleep_prev",
  "stroke_prev",
  "geolocation"
)

places = as_tibble(places_data)
write_csv(places, "cdc_places_data.csv")

## SVI Data

svi_dc = read.csv(str_c(data_dir, "dc_cdc_svi.csv"))
svi_md = read.csv(str_c(data_dir, "md_cdc_svi.csv"))
svi_va = read.csv(str_c(data_dir, "va_cdc_svi.csv"))

svi_data = rbind(svi_dc, svi_md, svi_va)

index = c(1:8, (1:158)[str_detect(colnames(svi_data), "E_")])

svi_data = svi_data[, index]
svi_data = svi_data[, -(28:36)]

colnames(svi_data)

colnames(svi_data) <- c(
  "state_fips",
  "state_name",
  "state_abbrev",
  "county_fips",
  "county_name",
  "fips",
  "location",
  "area_sqmi",
  "total_pop",
  "housing_units",
  "num_households",
  "poverty",
  "unemployment",
  "housing_cost_burden",
  "no_highschool",
  "uninsured",
  "age_over_65",
  "age_under_17",
  "disabilities",
  "single_parent",
  "limited_english",
  "minority",
  "multi_unit_housing",
  "mobile_homes",
  "crowded_housing",
  "no_vehicle",
  "group_quarters"
)

svi = as_tibble(svi_data)
write_csv(svi, "cdc_svi_data.csv")

## EJI Data

eji_files = list.files("./data", pattern = "eji", full.names = T)
eji_data = lapply(eji_files, read_csv, show_col_types = F)
eji_data = rbind(eji_data[[1]], eji_data[[2]], eji_data[[3]])

index = str_detect(colnames(eji_data), "EPL_|RPL_|SPL_|M_|F_", negate = T)
eji_data = eji_data[, index]

colnames(eji_data)

colnames(eji_data) <- c(
  "state_fips",
  "county_fips",
  "tract_code",
  "affgeoid",
  "fips",
  "name",
  "county",
  "state_abbrev",
  "state",
  "location",
  "total_pop",
  "daytime_pop",
  "ozone",
  "pm2.5",
  "diesel",
  "cancer_prob",
  "npl_proximity",
  "toxic_poximity",
  "tsd_proximity",
  "rmp_proximity",
  "coal_proximity",
  "lead_proximity",
  "park_proximity",
  "house_age",
  "walkability",
  "rail_proximity",
  "road_proximity",
  "airport_proximity",
  "watershed_proximity",
  "minority_per",
  "poverty200",
  "no_highschool",
  "unemployment",
  "renters",
  "housbdn",
  "uninsured",
  "no_internet",
  "age_over65",
  "age_under17",
  "disabilities",
  "limited_english",
  "mobile_homes",
  "group_quarters",
  "high_bp",
  "asthma",
  "cancer",
  "poor_mental",
  "diabetes"
)

length(unique(eji_data$fips))

eji = as_tibble(select(eji_data, "state_fips":"watershed_proximity")) %>%
  select(!c("state_fips", "county_fips", "tract_code", "name", "affgeoid", "county", "location"))

eji <- eji %>%
  group_by(fips) %>%
  summarize(
    # for every numeric column, take its mean (dropping NAs)
    across(
      where(is.numeric),
      ~ mean(.x, na.rm = TRUE)
    ),
    # if you have any non-numeric columns you still want to keep,
    # take the first value per group (or choose another summary fn)
    across(
      where(~ !is.numeric(.x)),
      ~ first(.x)
    ),
    .groups = "drop"
  )

write_csv(eji, "cdc_eji_data.csv")

# COMBINE DATA
# ____________

require(sf)
require(ggplot2)
require(tidyverse)
require(broom)
require(fields)
require(geoR)
require(tmap)
require(spdep)
require(sphet)
require(sp)
require(INLA)
require(gstat)
require(stringr)
require(tigris)

places = as_tibble(read.csv("cdc_places_data.csv"))
svi = as_tibble(read.csv("cdc_svi_data.csv"))
asthma = as_tibble(read.csv("asthma_equity_data.csv"))
eji = as_tibble(read.csv("cdc_eji_data.csv"))

head(places, 10)
head(svi, 10)
head(asthma, 10)
head(eji, 10)

svi_asthma = full_join(svi, asthma, by = c("fips", "location"))
places = places %>% rename(fips = tract_fips)
full_data = full_join(svi_asthma, places, by = "fips")
full_data = full_join(full_data, eji, by = "fips")

colnames(full_data)

dmv_data = as_tibble(full_data) %>%
  select(!contains(".y")) %>%
  select(!c("unw_cent_long", "unw_cent_lat", "state_desc", "state"))

#  Load DC census tracts
dc_tracts <- tracts(state = "DC", cb = TRUE, year = 2022)
md_tracts <- tracts(state = "MD", cb = TRUE, year = 2022)
va_tracts <- tracts(state = "VA", cb = TRUE, year = 2022)
tracts = rbind(dc_tracts, md_tracts, va_tracts) %>% rename(fips = GEOID) %>% mutate(fips = as.double(fips)) %>% select(!c("STATEFP", "COUNTYFP", "TRACTCE", "AFFGEOID", "NAME", "NAMELSAD", "STUSPS", "NAMELSADCO", "STATE_NAME", "LSAD", "ALAND", "AWATER"))

dmv_data = left_join(dmv_data, tracts, by = "fips")

ggplot(dmv_data, aes(geometry = geometry)) + geom_sf()

write_csv(dmv_data, "dmv_combined_data.csv")

# WRITE SHP FILES
# _______________

library(sf)
require(ggplot2)
require(tidyverse)
require(broom)
require(fields)
require(geoR)
require(tmap)
require(spdep)
require(sphet)
require(sp)
require(INLA)
require(gstat)
require(stringr)

gdb_list = list.files(path = "./data", pattern = ".gdb", full.names = T)
flood_list = lapply(gdb_list, st_read,layer = 'S_FLD_HAZ_AR')

dmv = c("dc", "md", "va")
for(i in 1:3){
  st_write(flood_list[[i]], str_c(dmv[i], "_nfhl.shp"))
}

# FLOOD RISK
# __________

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

# PLOTTING
# ________

require(readr)
require(sf)
require(ggplot2)
require(tidyverse)
require(broom)
require(fields)
require(geoR)
require(tmap)
require(spdep)
require(sphet)
require(sp)
require(INLA)
require(gstat)
require(stringr)
require(tigris)

dmv_data = read_csv("./data/dmv_combined_data.csv")
dmv_flood_risk = read_csv("./data/dmv_flood_risk_percent.csv")

head(dmv_data, 10)
head(dmv_flood_risk, 10)

## Data Summaries

summary(dmv_data)

summary(dmv_flood_risk)

## Demographic Plots

ggplot(data = dmv_data) + 
  geom_histogram(aes(x = black), bins = 35,  stat = "bin", fill = "deepskyblue") + 
  labs(title = "Histogram of Black", y = "Count", x = "Percentage Black")

ggplot(data = dmv_data) + 
  geom_histogram(aes(x = white), bins = 35,  stat = "bin", fill = "deepskyblue") + 
  labs(title = "Histogram of White", y = "Count", x = "Percentage White")

ggplot(data = dmv_data) + 
  geom_histogram(aes(x = total_population), bins = 35,  stat = "bin", fill = "deepskyblue") + 
  labs(title = "Histogram of Total Population", y = "Count", x = "Total Population")

ggplot(data = dmv_data) + 
  geom_histogram(aes(x = num_households), bins = 35,  stat = "bin", fill = "goldenrod") + 
  labs(title = "Histogram of Number of Households", y = "Count", x = "Number of Households")

ggplot(data = dmv_data) + 
  geom_histogram(aes(x = total_population), bins = 35,  stat = "bin", fill = "goldenrod") + 
  labs(title = "Histogram of Total Population", y = "Count", x = "Total Population")

ggplot(data = dmv_data) + 
  geom_histogram(aes(x = med_income), bins = 35,  stat = "bin", fill = "goldenrod") + 
  labs(title = "Histogram of Median Income", y = "Count", x = "Median Income")

ggplot(data = dmv_data) + 
  geom_histogram(aes(x = smoking_prev.x), bins = 35,  stat = "bin", fill = "goldenrod") + 
  labs(title = "Histogram of Smoking Prevalence", y = "Count", x = "Smoking Prevalence")

ggplot(data = dmv_data) + 
  geom_histogram(aes(x = adult_asthma_prev), bins = 35,  stat = "bin", fill = "goldenrod") + 
  labs(title = "Histogram of Adult Asthma Prevalence", y = "Count", x = "Adult Asthma Prevalence")

ggplot(data = dmv_data) + 
  geom_histogram(aes(x = physical_activity_prev), bins = 35,  stat = "bin", fill = "goldenrod") + 
  labs(title = "Histogram of Physical Activity Prevalence", y = "Count", x = "Physical Activity Prevalence")

ggplot(data = dmv_data) + 
  geom_histogram(aes(x = poverty), bins = 35,  stat = "bin", fill = "goldenrod") + 
  labs(title = "Histogram of\nBelow 150% Poverty", y = "Count", x = "Number Below 150% Poverty")

ggplot(data = dmv_data) + 
  geom_histogram(aes(x = adult_obesity_prev), bins = 35,  stat = "bin", fill = "goldenrod") + 
  labs(title = "Histogram of Adult Obesity Prevalence", y = "Count", x = "Adult Obesity Prevalence")

ggplot(data = dmv_data) + 
  geom_histogram(aes(x = uninsured), bins = 35,  stat = "bin", fill = "goldenrod") + 
  labs(title = "Histogram of Number Uninsured", y = "Count", x = "Number Uninsured")

## Environmental Plots

ggplot(data = dmv_data) + 
  geom_histogram(aes(x = particulate_matter), bins = 35,  stat = "bin", fill = "forestgreen") + 
  labs(title = "Histogram of Particulate Matter", y = "Count", x = "Particulate Matter")

ggplot(data = dmv_data) + 
  geom_histogram(aes(x = ozone), bins = 35,  stat = "bin", fill = "forestgreen") + 
  labs(title = "Histogram of Mean Days\nAbove O_3 Standard", y = "Count", x = "Mean Days\nAbove O_3 Standard")

ggplot(data = dmv_data) + 
  geom_histogram(aes(x = pm2.5), bins = 35,  stat = "bin", fill = "forestgreen") + 
  labs(title = "Histogram of Mean Days\nAbove PM2.5 Standard", y = "Count", x = "Mean Days\nAbove PM2.5 Standard")

ggplot(data = dmv_data) + 
  geom_histogram(aes(x = watershed_proximity), bins = 35,  stat = "bin", fill = "darkorchid") + 
  labs(title = "Histogram of Watershed Proximity", y = "Count", x = "Percentage Insecting Impacted Watershed")

ggplot(data = dmv_data) + 
  geom_histogram(aes(x = road_proximity), bins = 35,  stat = "bin", fill = "darkorchid") + 
  labs(title = "Histogram of Road Proximity", y = "Count", x = "Proportion within 1-Mile\nBuffer of Roads/Highways")

ggplot(data = dmv_data) + 
  geom_histogram(aes(x = rail_proximity), bins = 35,  stat = "bin", fill = "darkorchid") + 
  labs(title = "Histogram of Railway Proximity", y = "Count", x = "Proportion within 1-Mile\nBuffer of Railways")

ggplot(data = dmv_data) + 
  geom_histogram(aes(x = airport_proximity), bins = 35,  stat = "bin", fill = "darkorchid") + 
  labs(title = "Histogram of Airport Proximity", y = "Count", x = "Proportion within 1-Mile\nBuffer of Airport")

## Asthma plots

ggplot(data = dmv_data) + 
  geom_histogram(aes(x = asthma_prev), bins = 35,  stat = "bin", fill = "firebrick") + 
  labs(title = "Histogram of Asthma Prevalence", y = "Count", x = "Asthma Prevalence")

## Transformation Analysis

ggplot(data = dmv_data) + 
  geom_histogram(aes(x = sqrt(black)), bins = 35,  stat = "bin", fill = "deepskyblue") + 
  labs(title = "Histogram of Black", sub = 
         "Transformed", y = "Count", x = "Percentage Black")

ggplot(data = dmv_data) + 
  geom_histogram(aes(x = sqrt(med_income)), bins = 35,  stat = "bin", fill = "goldenrod") + 
  labs(title = "Histogram of Median Income", sub = 
         "Transformed", y = "Count", x = "Median Income")

ggplot(data = dmv_data) + 
  geom_histogram(aes(x = sqrt(poverty)), bins = 35,  stat = "bin", fill = "goldenrod") + 
  labs(title = "Histogram of\nBelow 150% Poverty", sub = 
         "Transformed", y = "Count", x = "Number Below 150% Poverty")

## Geographic Plots

dmv_flood_risk = dmv_flood_risk %>% rename(fips = GEOID)
dmv_data = dmv_data %>% select(!c("geometry"))
dmv = full_join(dmv_data, dmv_flood_risk, by = "fips")

dc_tracts <- tracts(state = "DC", cb = TRUE, year = 2022)
md_tracts <- tracts(state = "MD", cb = TRUE, year = 2022)
va_tracts <- tracts(state = "VA", cb = TRUE, year = 2022)
tracts = rbind(dc_tracts, md_tracts, va_tracts) %>% rename(fips = GEOID) %>% mutate(fips = as.double(fips)) %>% select(!c("STATEFP", "COUNTYFP", "TRACTCE", "AFFGEOID", "NAME", "NAMELSAD", "STUSPS", "NAMELSADCO", "STATE_NAME", "LSAD", "ALAND", "AWATER"))

dmv = left_join(dmv, tracts, by = "fips")

## Demographic Variables

ggplot(dmv, aes(geometry = geometry, fill = no_highschool)) + geom_sf(col = NA) + coord_sf() + scale_fill_viridis_c() + labs(title = "No High-School Diploma", subtitle = "DMV Area", xlab = "Longitude", ylab = "Latitude", fill = "Prevalence") + theme(legend.position = "top", legend.title.position = "top", plot.margin = unit(c(0.05, 0.05, 0.05, 0.05), "cm"))

poverty_plot = ggplot(dmv, aes(geometry = geometry, fill = sqrt(poverty))) + geom_sf(col = NA) + coord_sf() + scale_fill_viridis_c() + labs(title = "Number Under 150% Poverty", subtitle = "DMV Area", xlab = "Longitude", ylab = "Latitude", fill = "Sqrt Number") + theme(legend.position = "top", legend.title.position = "top", plot.margin = unit(c(0.05, 0.05, 0.05, 0.05), "cm"))

ggsave("./plots/poverty_plot.png", plot = poverty_plot)

unemployment_plot = ggplot(dmv, aes(geometry = geometry, fill = unemployment)) + geom_sf(col = NA) + coord_sf() + scale_fill_viridis_c() + labs(title = "Unemployment Rate", subtitle = "DMV Area", xlab = "Longitude", ylab = "Latitude", fill = "Prevalence") + theme(legend.position = "top", legend.title.position = "top", plot.margin = unit(c(0.05, 0.05, 0.05, 0.05), "cm"))

ggsave("./plots/unemployment_plot.png", plot = unemployment_plot)

nonwhite_plot = ggplot(dmv, aes(geometry = geometry, fill = non_white)) + geom_sf(col = NA) + coord_sf() + scale_fill_viridis_c() + labs(title = "Non-White Percentage", subtitle = "DMV Area", xlab = "Longitude", ylab = "Latitude", fill = "Percentage") + theme(legend.position = "top", legend.title.position = "top", plot.margin = unit(c(0.05, 0.05, 0.05, 0.05), "cm"))

ggsave("./plots/nonwhite_plot.png", plot = nonwhite_plot)

nonwhite_plot = ggplot(dmv, aes(geometry = geometry, fill = sqrt(black))) + geom_sf(col = NA) + coord_sf() + scale_fill_viridis_c() + labs(title = "Black Percentage", subtitle = "DMV Area", xlab = "Longitude", ylab = "Latitude", fill = "Sqrt Percentage") + theme(legend.position = "top", legend.title.position = "top", plot.margin = unit(c(0.05, 0.05, 0.05, 0.05), "cm"))

ggsave("./plots/black_plot.png", plot = nonwhite_plot)

minority_plot = ggplot(dmv, aes(geometry = geometry, fill = minority)) + geom_sf(col = NA) + coord_sf() + scale_fill_viridis_c() + labs(title = "Minority Prevalence", subtitle = "DMV Area", xlab = "Longitude", ylab = "Latitude", fill = "Prevalence") + theme(legend.position = "top", legend.title.position = "top", plot.margin = unit(c(0.05, 0.05, 0.05, 0.05), "cm"))

ggsave("./plots/minority_plot.png", plot = minority_plot)

income_plot = ggplot(dmv, aes(geometry = geometry, fill = sqrt(med_income))) + geom_sf(col = NA) + coord_sf() + scale_fill_viridis_c() + labs(title = "Median Household Income", subtitle = "DMV Area", xlab = "Longitude", ylab = "Latitude", fill = "Sqrt Median Income") + theme(legend.position = "top", legend.title.position = "top", plot.margin = unit(c(0.05, 0.05, 0.05, 0.05), "cm"))

ggsave("./plots/income_plot.png", plot = income_plot)

ggplot(dmv, aes(geometry = geometry, fill = est_population)) + geom_sf(col = NA) + coord_sf() + scale_fill_viridis_c() + labs(title = "Estimated Population", subtitle = "DMV Area", xlab = "Longitude", ylab = "Latitude", fill = "Population") + theme(legend.position = "top", legend.title.position = "top", plot.margin = unit(c(0.05, 0.05, 0.05, 0.05), "cm"))

## Environmental Variables

flood_plot = ggplot(dmv, aes(geometry = geometry, fill = percent_in_flood_zone)) + geom_sf(col = NA) + coord_sf() + scale_fill_viridis_c() + labs(title = "Percent in Flood Zone", subtitle = "DMV Area", xlab = "Longitude", ylab = "Latitude", fill = "Percent") + theme(legend.position = "top", legend.title.position = "top", plot.margin = unit(c(0.05, 0.05, 0.05, 0.05), "cm"))

ggsave("./plots/flood_plot.png", plot = flood_plot)

pm25_plot = ggplot(dmv, aes(geometry = geometry, fill = pm2.5)) + geom_sf(col = NA) + coord_sf() + scale_fill_viridis_c() + labs(title = "PM2.5 Burden", subtitle = "DMV Area", xlab = "Longitude", ylab = "Latitude", fill = "Mean Days Above Standard") + theme(legend.position = "top", legend.title.position = "top", plot.margin = unit(c(0.05, 0.05, 0.05, 0.05), "cm"))

ggsave("./plots/pm25_plot.png", plot = pm25_plot)

ozone_plot = ggplot(dmv, aes(geometry = geometry, fill = ozone)) + geom_sf(col = NA) + coord_sf() + scale_fill_viridis_c() + labs(title = "Ozone Burden", subtitle = "DMV Area", xlab = "Longitude", ylab = "Latitude", fill = "Mean Days Above Standard") + theme(legend.position = "top", legend.title.position = "top", plot.margin = unit(c(0.05, 0.05, 0.05, 0.05), "cm"))

ggsave("./plots/ozone_plot.png", plot = ozone_plot)

particle_plot = ggplot(dmv, aes(geometry = geometry, fill = particulate_matter)) + geom_sf(col = NA) + coord_sf() + scale_fill_viridis_c() + labs(title = "Particulate Matter", subtitle = "DMV Area", xlab = "Longitude", ylab = "Latitude", fill = "Concentration") + theme(legend.position = "top", legend.title.position = "top", plot.margin = unit(c(0.05, 0.05, 0.05, 0.05), "cm"))

ggsave("./plots/particule_plot.png", plot = particle_plot)

road_plot = ggplot(dmv, aes(geometry = geometry, fill = road_proximity)) + geom_sf(col = NA) + coord_sf() + scale_fill_viridis_c() + labs(title = "Proportion within 1-Mile\nBuffer of Roads/Highways", subtitle = "DMV Area", xlab = "Longitude", ylab = "Latitude", fill = "Proportion") + theme(legend.position = "top", legend.title.position = "top", plot.margin = unit(c(0.05, 0.05, 0.05, 0.05), "cm"))

ggsave("./plots/road_plot.png", plot = road_plot)

rail_plot = ggplot(dmv, aes(geometry = geometry, fill = rail_proximity)) + geom_sf(col = NA) + coord_sf() + scale_fill_viridis_c() + labs(title = "Proportion within 1-Mile\nBuffer of Railways", subtitle = "DMV Area", xlab = "Longitude", ylab = "Latitude", fill = "Proportion") + theme(legend.position = "top", legend.title.position = "top", plot.margin = unit(c(0.05, 0.05, 0.05, 0.05), "cm"))

ggsave("./plots/rail_plot.png", plot = rail_plot)

airport_plot = ggplot(dmv, aes(geometry = geometry, fill = airport_proximity)) + geom_sf(col = NA) + coord_sf() + scale_fill_viridis_c() + labs(title = "Proportion within 1-Mile\nBuffer of Airports", subtitle = "DMV Area", xlab = "Longitude", ylab = "Latitude", fill = "Proportion") + theme(legend.position = "top", legend.title.position = "top", plot.margin = unit(c(0.05, 0.05, 0.05, 0.05), "cm"))

ggsave("./plots/airport_plot.png", plot = airport_plot)

asthma_plot = ggplot(dmv, aes(geometry = geometry, fill = asthma_prev)) + geom_sf(col = NA) + coord_sf() + scale_fill_viridis_c() + labs(title = "Asthma Prevalence", subtitle = "DMV Area", xlab = "Longitude", ylab = "Latitude", fill = "Prevalence") + theme(legend.position = "top", legend.title.position = "top", plot.margin = unit(c(0.05, 0.05, 0.05, 0.05), "cm"))

ggsave("./plots/asthma_plot.png", plot = asthma_plot)

# RANDOM FOREST
# _____________

require(readr)
require(sf)
require(ggplot2)
require(tidyverse)
require(broom)
require(fields)
require(geoR)
require(tmap)
require(spdep)
require(sphet)
require(sp)
require(INLA)
require(gstat)
require(stringr)
require(tigris)
require(randomForest)

## Load data

dmv_data = read_csv("./data/dmv_combined_data.csv")
dmv_flood_risk = read_csv("./data/dmv_flood_risk_percent.csv")

dmv_flood_risk = dmv_flood_risk %>% rename(fips = GEOID)
dmv_data = dmv_data %>% select(!c("geometry"))
dmv = full_join(dmv_data, dmv_flood_risk, by = "fips")

dc_tracts <- tracts(state = "DC", cb = TRUE, year = 2022)
md_tracts <- tracts(state = "MD", cb = TRUE, year = 2022)
va_tracts <- tracts(state = "VA", cb = TRUE, year = 2022)
tracts = rbind(dc_tracts, md_tracts, va_tracts) %>% rename(fips = GEOID) %>% mutate(fips = as.double(fips)) %>% select(!c("STATEFP", "COUNTYFP", "TRACTCE", "AFFGEOID", "NAME", "NAMELSAD", "STUSPS", "NAMELSADCO", "STATE_NAME", "LSAD", "ALAND", "AWATER"))

dmv = left_join(dmv, tracts, by = "fips")

## Split Data
set.seed(6289)
sample <- sample(c(TRUE, FALSE), nrow(dmv), replace=TRUE, prob=c(0.8,0.2))
dmv_train  <- dmv[sample, ]
dmv_test   <- dmv[!sample, ]

## Random Forest
set.seed(6289)
dmv_data = select(dmv_train, "area_sqmi":"flood_risk")
dmv_rf = randomForest(asthma_prev ~ ., data = dmv_data, importance = T, na.action = na.exclude)
dmv_rf

plot(dmv_rf)

dmv_test_data <- dmv_test %>% select(area_sqmi:flood_risk)

# Drop rows with missing values (if any)
dmv_test_data <- na.omit(dmv_test_data)

# Predict using cleaned test data
test_pred <- predict(dmv_rf, newdata = dmv_test_data)

# Align actual values (only for non-NA rows)
test_actual <- dmv_test$asthma_prev[as.numeric(rownames(dmv_test_data))]

# Compute RMSE (Root Mean Squared Error)
rmse <- sqrt(mean((test_actual - test_pred)^2))

# Compute R-squared (proportion of variance explained)
r_squared <- 1 - sum((test_actual - test_pred)^2) / sum((test_actual - mean(test_actual))^2)

# Print results
cat("Test RMSE: ", round(rmse, 3), "\n")
cat("Test R-squared: ", round(r_squared, 3), "\n")

## Variable Importance

importance(dmv_rf, type = 1)
varImpPlot(dmv_rf, type = 1)

## Residuals

dmv_res = residuals(dmv_rf)

# Added: Residual Plot
# Compute residuals manually

fitted_vals <- predict(dmv_rf)  # This uses training data internally
actual_vals <- dmv_data$asthma_prev  # Training set actual values
residuals_rf <- actual_vals - fitted_vals

# Now safe to combine into a dataframe

resid_df <- data.frame(Fitted = fitted_vals, Residuals = residuals_rf)

# Plot residuals

ggplot(resid_df, aes(x = Fitted, y = Residuals)) +
  geom_point(alpha = 0.5) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  labs(title = "Residuals vs Fitted Values",
       x = "Fitted Values",
       y = "Residuals") +
  theme_minimal()

## Predictions

dmv_pred = predict(dmv_rf, cpkgs = "randomForest")
# dmv$asthma_pred = dmv_pred
dmv$asthma_pred <- NA
dmv$asthma_pred[as.numeric(rownames(dmv_train))] <- dmv_pred

# ggplot(dmv, aes(geometry = geometry, fill = asthma_pred)) + geom_sf(col = NA) + coord_sf() + scale_fill_viridis_c() + labs(title = "Asthma Predictions", subtitle = "DMV Area", xlab = "Longitude", ylab = "Latitude", fill = "Prevalence") + theme(legend.position = "top", legend.title.position = "top", plot.margin = unit(c(0.05, 0.05, 0.05, 0.05), "cm"))

ggplot(dmv, aes(geometry = geometry, fill = asthma_pred)) +
  geom_sf(color = NA) +
  coord_sf() +
  scale_fill_viridis_c() +
  labs(
    title = "Asthma Predictions for Version 1",
    subtitle = "DMV Area",
    xlab = "Longitude",
    ylab = "Latitude",
    fill = "Predicted Prevalence"
  ) +
  theme(
    legend.position = "top",
    legend.title.position = "top",
    plot.margin = unit(c(0.05, 0.05, 0.05, 0.05), "cm")
  )

# FINAL VARS SELECTION
# ____________________

require(readr)
require(sf)
require(ggplot2)
require(tidyverse)
require(broom)
require(fields)
require(geoR)
require(tmap)
require(spdep)
require(sphet)
require(sp)
require(INLA)
require(gstat)
require(stringr)
require(tigris)
require(randomForest)
require(corrr)
require(dplyr)
require(tidyr)

## Load data

dmv_data = read_csv("./data/dmv_combined_data.csv")
dmv_flood_risk = read_csv("./data/dmv_flood_risk_percent.csv")

dmv_flood_risk = dmv_flood_risk %>% rename(fips = GEOID)
dmv_data = dmv_data %>% select(!c("geometry"))
dmv = full_join(dmv_data, dmv_flood_risk, by = "fips")

dc_tracts <- tracts(state = "DC", cb = TRUE, year = 2022)
md_tracts <- tracts(state = "MD", cb = TRUE, year = 2022)
va_tracts <- tracts(state = "VA", cb = TRUE, year = 2022)
tracts = rbind(dc_tracts, md_tracts, va_tracts) %>% rename(fips = GEOID) %>% mutate(fips = as.double(fips)) %>% select(!c("STATEFP", "COUNTYFP", "TRACTCE", "AFFGEOID", "NAME", "NAMELSAD", "STUSPS", "NAMELSADCO", "STATE_NAME", "LSAD", "ALAND", "AWATER"))

dmv = left_join(dmv, tracts, by = "fips")

## Random Forest

## Split Data
set.seed(6289)
sample <- sample(c(TRUE, FALSE), nrow(dmv), replace=TRUE, prob=c(0.8,0.2))
dmv_train  <- dmv[sample, ]
dmv_test   <- dmv[!sample, ]

## Random Forest
set.seed(6289)
dmv_data = select(dmv_train, "area_sqmi":"flood_risk")
### Added: Convert all character columns to factor (before modeling)
# Step 1: Select numeric + factor modeling variables only
dmv_data <- dmv_train %>%
  dplyr::select(area_sqmi:flood_risk)

# Step 2: Convert character columns to factor
dmv_data <- dmv_data %>%
  dplyr::mutate(across(where(is.character), as.factor))

# Step 3: Drop high-cardinality factor variables (nlevels > 53)
too_many_levels <- sapply(dmv_data, function(x) is.factor(x) && nlevels(x) > 53)
dmv_data <- dmv_data[, !too_many_levels]

# Step 4: (Optional) confirm no illegal variables remain
str(dmv_data)  # or View(dmv_data)

sapply(dmv_data, nlevels)
sapply(dmv_data, function(x) if (is.factor(x)) nlevels(x) else NA)

# ###
dmv_rf = randomForest(asthma_prev ~ ., data = dmv_data, importance = T, na.action = na.roughfix)
dmv_rf
plot(dmv_rf)

### Tuning
set.seed(6289)

# Tune mtry (number of variables tried at each split)
# Tune nodesize (minimum size of terminal nodes)
# Tune maxnodes (maximum number of terminal nodes per tree)

dmv_rf_tuned <- randomForest(
  asthma_prev ~ .,
  data = dmv_data,
  ntree = 500,          # number of trees
  mtry = 15,             # try mtry = 15, adjust depending on # of predictors
  nodesize = 10,        # larger nodes = less overfitting
  maxnodes = 50,        # limit tree depth
  importance = TRUE,
  na.action = na.roughfix
)

# Compare training performance
print(dmv_rf)         # original model
print(dmv_rf_tuned)   # tuned model

# OOB Error vs Number of Trees
plot(dmv_rf_tuned$mse, type = "l", col = "blue", lwd = 2,
     ylab = "OOB MSE", xlab = "Number of Trees",
     main = "OOB Error vs Number of Trees")

plot(dmv_rf)


### Added: The performance of the model on the test set

# Match the same structure as the training data
dmv_test_data <- dmv_test %>%
  select(area_sqmi:flood_risk) %>% 
  mutate(across(where(is.character), as.factor)) # Ensure factor types

# Drop high-cardinality factors (same rule as training)
too_many_levels <- sapply(dmv_test_data, function(x) is.factor(x) && nlevels(x) > 53)
dmv_test_data <- dmv_test_data[, !too_many_levels]

# Match column order to training data
dmv_test_data <- dmv_test_data[, colnames(dmv_data)]

# Fill missing values the same way as training (na.roughfix)
dmv_test_data <- na.roughfix(dmv_test_data)

# ### R_squared negative version ###
# # Predict asthma prevalence on the test set
# test_pred <- predict(dmv_rf, newdata = dmv_test)
# 
# # Extract actual values from the test set
# test_actual <- dmv_test$asthma_prev
# 
# # Compute RMSE (Root Mean Squared Error)
# rmse <- sqrt(mean((test_actual - test_pred)^2))
# 
# # Compute R-squared
# r_squared <- 1 - sum((test_actual - test_pred)^2) / sum((test_actual - mean(test_actual))^2)
# 
# # Print results
# cat("Test RMSE: ", round(rmse, 3), "\n")
# cat("Test R-squared: ", round(r_squared, 3), "\n")
# #######

# Step 1: Predict both models
test_pred <- predict(dmv_rf, newdata = dmv_test_data)
test_pred_tuned <- predict(dmv_rf_tuned, newdata = dmv_test_data)

# Step 2: Get actual values
test_actual <- dmv_test$asthma_prev[as.numeric(rownames(dmv_test_data))]

# Step 3: Identify complete cases across all three vectors
non_missing <- complete.cases(test_actual, test_pred, test_pred_tuned)

# Step 4: Subset all three
test_actual <- test_actual[non_missing]
test_pred <- test_pred[non_missing]
test_pred_tuned <- test_pred_tuned[non_missing]

# Step 5: Compute metrics for both models
rmse <- sqrt(mean((test_actual - test_pred)^2))
r_squared <- 1 - sum((test_actual - test_pred)^2) / sum((test_actual - mean(test_actual))^2)
relative_rmse <- (rmse / mean(test_actual)) * 100

rmse_tuned <- sqrt(mean((test_actual - test_pred_tuned)^2))
r_squared_tuned <- 1 - sum((test_actual - test_pred_tuned)^2) / sum((test_actual - mean(test_actual))^2)
relative_rmse_tuned <- (rmse_tuned / mean(test_actual)) * 100

# Step 6: Print results
cat("Original RF Test RMSE:", round(rmse, 3), "\n")
cat("Original RF Test R-squared:", round(r_squared, 3), "\n")
cat("Original RF Relative RMSE (%):", round(relative_rmse, 2), "%\n\n")

cat("Tuned RF Test RMSE:", round(rmse_tuned, 3), "\n")
cat("Tuned RF Test R-squared:", round(r_squared_tuned, 3), "\n")
cat("Tuned RF Relative RMSE (%):", round(relative_rmse_tuned, 2), "%\n")

# 
# 
# # Predict
# test_pred <- predict(dmv_rf, newdata = dmv_test_data)
# test_pred_tuned <- predict(dmv_rf_tuned, newdata = dmv_test_data)
# 
# non_missing <- !is.na(test_actual)
# test_actual_tuned <- test_actual[non_missing]
# test_pred_tuned <- test_pred_tuned[non_missing]
# # Align actual values
# test_actual <- dmv_test$asthma_prev[as.numeric(rownames(dmv_test_data))]
# 
# # Compute RMSE and R-squared
# rmse <- sqrt(mean((test_actual - test_pred)^2))
# r_squared <- 1 - sum((test_actual - test_pred)^2) / sum((test_actual - mean(test_actual))^2)
# rmse_tuned <- sqrt(mean((test_actual - test_pred_tuned)^2))
# r_squared_tuned <- 1 - sum((test_actual - test_pred_tuned)^2) / sum((test_actual - mean(test_actual))^2)
# 
# # Also show relative RMSE
# relative_rmse <- (rmse / mean(test_actual)) * 100
# relative_rmse_tuned <- (rmse_tuned / mean(test_actual)) * 100
# 
# # Print results
# cat("Original RF Test RMSE:", round(rmse, 3), "\n")
# cat("Original RF Test R-squared:", round(r_squared, 3), "\n")
# cat("Original RF Relative RMSE (%):", round(relative_rmse, 2), "%\n")
# 
# cat("Tuned RF Test RMSE:", round(rmse_tuned, 3), "\n")
# cat("Tuned RF Test R-squared:", round(r_squared_tuned, 3), "\n")
# cat("Tuned RF Relative RMSE (%):", round(relative_rmse_tuned, 2), "%\n")
# 
# # Extract actual values for the same rows in test data
# test_actual_full <- dmv_test$asthma_prev[as.numeric(rownames(dmv_test_data))]
# 
# # Filter out rows where asthma_prev is NA
# non_missing <- !is.na(test_actual_full)
# test_actual <- test_actual_full[non_missing]
# test_pred <- test_pred[non_missing]
# 
# # Compute RMSE and R²
# rmse <- sqrt(mean((test_actual - test_pred)^2))
# r_squared <- 1 - sum((test_actual - test_pred)^2) / sum((test_actual - mean(test_actual))^2)
# rmse_tuned <- sqrt(mean((test_actual - test_pred_tuned)^2))
# r_squared_tuned <- 1 - sum((test_actual - test_pred_tuned)^2) / sum((test_actual - mean(test_actual))^2)
# 
# 
# # Output results
# cat("Original RF Test RMSE:", round(rmse, 3), "\n")
# cat("Original RF Test R-squared:", round(r_squared, 3), "\n")
# cat("Original RF Relative RMSE (%):", round(relative_rmse, 2), "%\n")
# 
# cat("Tuned RF Test RMSE:", round(rmse_tuned, 3), "\n")
# cat("Tuned RF Test R-squared:", round(r_squared_tuned, 3), "\n")
# cat("Tuned RF Relative RMSE (%):", round(relative_rmse_tuned, 2), "%\n")

## Variable Importance

importance_values <- importance(dmv_rf, type = 1)
print(importance_values)

library(randomForest)
library(tibble)
library(dplyr)
library(ggplot2)

imp_df <- as_tibble(importance(dmv_rf, type = 1), rownames = "Variable") %>%
  arrange(desc(`%IncMSE`)) %>%
  slice(1:15)

ggplot(imp_df, aes(x = reorder(Variable, `%IncMSE`), y = `%IncMSE`)) +
  geom_point(size = 3) +
  coord_flip() +
  labs(
    title = "Top 15 Important Variables (%IncMSE)",
    x = "Variable",
    y = "% Increase in MSE"
  ) +
  theme_minimal(base_size = 12)


# top_10_vars <- names(sort(importance_values[, 1], decreasing = TRUE))[1:10]
# print(top_10_vars)
# dmv_top_10_vars_data <- dmv[, c("asthma_prev", top_10_vars)]
# write.csv(dmv_top_10_vars_data, "dmv_top_10_vars_data.csv", row.names = FALSE)
# top_20_vars <- names(sort(importance_values[, 1], decreasing = TRUE))[1:20]
# dmv_top_20_vars_data <- dmv_data[, c("asthma_prev", top_20_vars)]
# write.csv(dmv_top_20_vars_data, "dmv_top_20_vars_data.csv", row.names = FALSE)

## Residuals

dmv_res = residuals(dmv_rf)

# Added: Residual Plot
# Compute residuals manually
fitted_vals <- predict(dmv_rf)  # This uses training data internally
actual_vals <- dmv_data$asthma_prev  # Training set actual values
residuals_rf <- actual_vals - fitted_vals

# Now safe to combine into a dataframe
resid_df <- data.frame(Fitted = fitted_vals, Residuals = residuals_rf)

# Plot residuals
ggplot(resid_df, aes(x = Fitted, y = Residuals)) +
  geom_point(alpha = 0.5) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  labs(title = "Residuals vs Fitted Values",
       x = "Fitted Values",
       y = "Residuals") +
  theme_minimal()

## Predictions

dmv_pred = predict(dmv_rf, cpkgs = "randomForest")
# dmv_data$asthma_pred = dmv_pred
dmv$asthma_pred <- NA
dmv$asthma_pred[as.numeric(rownames(dmv_train))] <- dmv_pred

# ggplot(dmv, aes(geometry = geometry, fill = asthma_pred)) + geom_sf(col = NA) + coord_sf() + scale_fill_viridis_c() + labs(title = "Asthma Predictions", subtitle = "DMV Area", xlab = "Longitude", ylab = "Latitude", fill = "Prevalence") + theme(legend.position = "top", legend.title.position = "top", plot.margin = unit(c(0.05, 0.05, 0.05, 0.05), "cm"))

ggplot(dmv, aes(geometry = geometry, fill = asthma_pred)) +
  geom_sf(color = NA) +
  coord_sf() +
  scale_fill_viridis_c() +
  labs(
    title = "Asthma Predictions",
    subtitle = "DMV Area",
    xlab = "Longitude",
    ylab = "Latitude",
    fill = "Predicted Prevalence"
  ) +
  theme(
    legend.position = "top",
    legend.title.position = "top",
    plot.margin = unit(c(0.05, 0.05, 0.05, 0.05), "cm")
  )

## VIF_1

vif_df <- dmv %>%
  select(asthma_prev, area_sqmi:flood_risk) %>%
  drop_na() %>%
  select(where(is.numeric)) %>%
  select(where(~ var(., na.rm = TRUE) > 0))

X <- model.matrix(asthma_prev ~ ., data = vif_df)


qrX <- qr(X)
independent_cols <- qrX$pivot[1:qrX$rank]


vif_df_clean <- vif_df[, c(1, independent_cols[-1])]  


lm_vif_clean <- lm(asthma_prev ~ ., data = vif_df_clean)
vif_values <- car::vif(lm_vif_clean)
print(vif_values)

x_vars <- dmv %>%
  dplyr::select(-asthma_prev) %>%
  dplyr::select(where(is.numeric)) %>%
  na.omit()

constant_vars <- sapply(x_vars, function(x) sd(x, na.rm = TRUE) == 0)
x_vars_clean <- x_vars[, !constant_vars]
names(constant_vars[constant_vars])
cor_matrix <- cor(x_vars_clean, use = "pairwise.complete.obs")

dist_matrix <- as.dist(1 - abs(cor_matrix))
hc <- hclust(dist_matrix, method = "average")
plot(hc, main = "Variable Clustering Based on Correlation")

groups <- cutree(hc, h = 0.1)  

library(dplyr)


group_df <- data.frame(
  Variable = names(groups),
  Group = groups
)

importance_df <- importance_values %>%
  as.data.frame() %>%
  tibble::rownames_to_column("Variable")

group_df <- group_df %>%
  left_join(importance_df, by = "Variable")

selection_vif_1 <- group_df %>%
  group_by(Group) %>%
  slice_max(`%IncMSE`, n = 1) %>%
  pull(Variable)

print(selection_vif_1)

## VIF_2

# ---- Step 1: VIF check on selection_vif_1 ----
vars_to_use <- intersect(c(selection_vif_1, "asthma_prev"), colnames(dmv))
vif_data <- dmv[, vars_to_use, drop = FALSE]
vif_data <- tidyr::drop_na(vif_data)

lm_vif <- lm(asthma_prev ~ ., data = vif_data)
vif_vals <- car::vif(lm_vif)
high_vif_vars <- names(vif_vals[vif_vals > 5])
cat("VIF>5 removed vars (to be group-rescanned):\n", high_vif_vars, "\n")

# ---- Step 2: Dynamically regroup current variable set ----
vars_now <- selection_vif_1
cor_matrix <- cor(dmv %>% select(all_of(vars_now)), use = "pairwise.complete.obs")
dist_matrix <- as.dist(1 - abs(cor_matrix))
hc <- hclust(dist_matrix, method = "average")

# Plot the dendrogram
plot(hc, main = "VIF_2: Variable Clustering Based on Correlation")

groups <- cutree(hc, h = 0.1)
group_df <- data.frame(
  Variable = names(groups),
  Group = groups
)

# ---- Step 3: Replace high VIF variables with non-redundant new variables ----
group_selection_df <- group_df %>%
  filter(Variable %in% selection_vif_1)

replacement_vars <- high_vif_vars %>%
  map_chr(function(v) {
    g <- group_df$Group[group_df$Variable == v]
    group_members <- group_df %>% filter(Group == g)
    candidates <- group_members %>%
      filter(!(Variable %in% high_vif_vars)) %>%
      filter(!(Variable %in% selection_vif_1)) %>%
      filter(Variable %in% colnames(dmv))
    if (nrow(candidates) == 0) return(NA_character_)
    candidates %>% slice_max(`%IncMSE`, n = 1) %>% pull(Variable)
  }) %>% na.omit()

# ---- Step 4: New selection = old minus high VIF + replacements ----
selection_vif_2 <- union(setdiff(selection_vif_1, high_vif_vars), replacement_vars)

# ---- Step 5: Summary of change ----
cat("Original variable count:", length(selection_vif_1), "\n")
cat("Removed (VIF > 5):", length(high_vif_vars), "\n")
cat("Replaced with:", length(replacement_vars), "\n")
cat("New selection size:", length(selection_vif_2), "\n")

print(selection_vif_2)

## VIF_3

# ---- Step 1: VIF check on selection_vif_2 ----
vars_to_use <- intersect(c(selection_vif_2, "asthma_prev"), colnames(dmv))
vif_data <- dmv[, vars_to_use, drop = FALSE]
vif_data <- tidyr::drop_na(vif_data)

lm_vif <- lm(asthma_prev ~ ., data = vif_data)
vif_vals <- car::vif(lm_vif)
high_vif_vars <- names(vif_vals[vif_vals > 5])
cat("VIF>5 removed vars (to be group-rescanned):\n", high_vif_vars, "\n")

# ---- Step 2: Dynamically regroup current variable set ----
vars_now <- selection_vif_2
cor_matrix <- cor(dmv %>% select(all_of(vars_now)), use = "pairwise.complete.obs")
dist_matrix <- as.dist(1 - abs(cor_matrix))
hc <- hclust(dist_matrix, method = "average")

# Plot dendrogram for current variables
plot(hc, main = "VIF_3: Variable Clustering Based on Correlation")

groups <- cutree(hc, h = 0.1)
group_df <- data.frame(
  Variable = names(groups),
  Group = groups
)

# ---- Step 3: Replace high VIF variables with non-redundant new variables ----
group_selection_df <- group_df %>%
  filter(Variable %in% selection_vif_2)

replacement_vars <- high_vif_vars %>%
  map_chr(function(v) {
    g <- group_df$Group[group_df$Variable == v]
    group_members <- group_df %>% filter(Group == g)
    candidates <- group_members %>%
      filter(!(Variable %in% high_vif_vars)) %>%
      filter(!(Variable %in% selection_vif_2)) %>%
      filter(Variable %in% colnames(dmv))
    if (nrow(candidates) == 0) return(NA_character_)
    candidates %>% slice_max(`%IncMSE`, n = 1) %>% pull(Variable)
  }) %>% na.omit()

# ---- Step 4: New selection = old minus high VIF + replacements ----
selection_vif_3 <- union(setdiff(selection_vif_2, high_vif_vars), replacement_vars)

# ---- Step 5: Summary of change ----
cat("Original variable count:", length(selection_vif_2), "\n")
cat("Removed (VIF > 5):", length(high_vif_vars), "\n")
cat("Replaced with:", length(replacement_vars), "\n")
cat("New selection size:", length(selection_vif_3), "\n")

print(selection_vif_3)

## Final Selection

final_top <- importance_df %>%
  filter(Variable %in% selection_vif_3) %>%
  slice_max(`%IncMSE`, n = 15) %>%
  pull(Variable)

pollution_top <- importance_df %>%
  filter(Variable %in% c("pm2.5", "ozone", "diesel")) %>%
  slice_max(`%IncMSE`, n = 1) %>%
  pull(Variable)

final_vars <- union(final_top, c("percent_in_flood_zone", pollution_top))

print(final_vars)

model_data <- dmv %>%
  select(all_of(c("asthma_prev", final_vars))) %>%
  drop_na()

# Export

model_data <- dmv %>%
  select(all_of(c("fips", "asthma_prev", final_vars))) %>%
  drop_na()

write.csv(model_data, "final_model_data.csv", row.names = FALSE)

## Visualization for the Final Selection

library(knitr)

var_table <- data.frame(
  Variable = c("ozone", "cancer_prob", "watershed_proximity", "area_sqmi", "house_age", "walkability",
               "percent_in_flood_zone", "asian", "hispanic", "two_or_more", "limited_english",
               "uninsured", "unemployment", "labor_participation", "single_parent",
               "mobile_homes", "housing_stress"),
  Category = c("Environmental", "Environmental", "Environmental", "Environmental", "Environmental", "Environmental",
               "Flood", "Social", "Social", "Social", "Social",
               "Social", "Social", "Social", "Social",
               "Social", "Social")
)

kable(var_table, caption = "Final Variable Classification and Interpretation", format = "pipe")
View(var_table)

library(dplyr)
cor_df <- dmv %>%
  select(asthma_prev, all_of(final_vars)) %>%
  cor(method = "spearman", use = "pairwise.complete.obs") %>%
  as.data.frame() %>%
  select(asthma_prev) %>%
  rownames_to_column("Variable") %>%
  filter(Variable != "asthma_prev")

ggplot(cor_df, aes(x = reorder(Variable, asthma_prev), y = asthma_prev)) +
  geom_col(fill = "steelblue") +
  coord_flip() +
  labs(title = "Spearman Correlation with Asthma Prevalence", y = "Correlation", x = NULL) +
  theme_minimal()

# MODELLING
# __________

# Step 1: Data Setup and Exploration

# ───────────────────────────────────────────────────────────────────────────────
# 0. Install & load required packages
# ───────────────────────────────────────────────────────────────────────────────
pkgs <- c("sf", "dplyr", "readr", "tmap", "spdep", "tigris", "stringr")
to_install <- pkgs[!sapply(pkgs, requireNamespace, quietly = TRUE)]
if(length(to_install)) install.packages(to_install, repos = "https://cloud.r-project.org")
lapply(pkgs, library, character.only = TRUE)
options(tigris_use_cache = TRUE)

# ───────────────────────────────────────────────────────────────────────────────
# 1. Read DMV attribute table & standardize FIPS
# ───────────────────────────────────────────────────────────────────────────────
dmv_data <- read_csv(
  "./final_model_data.csv",
  show_col_types = FALSE
) %>%
  mutate(fips = str_pad(as.character(fips), width = 11, pad = "0"))

# ───────────────────────────────────────────────────────────────────────────────
# 2. Download & bind Census tract boundaries for DC, MD, VA
# ───────────────────────────────────────────────────────────────────────────────
dc_tracts <- tracts(state = "DC", year = 2022, cb = TRUE)
md_tracts <- tracts(state = "MD", year = 2022, cb = TRUE)
va_tracts <- tracts(state = "VA", year = 2022, cb = TRUE)

tracts <- bind_rows(dc_tracts, md_tracts, va_tracts) %>%
  mutate(GEOID = str_pad(GEOID, width = 11, pad = "0"))

# ───────────────────────────────────────────────────────────────────────────────
# 3. Join attributes onto tract geometries
# ───────────────────────────────────────────────────────────────────────────────
dmv_sf <- tracts %>%
  left_join(dmv_data, by = c("GEOID" = "fips"))

# ───────────────────────────────────────────────────────────────────────────────
# 4. Filter & map asthma prevalence
# ───────────────────────────────────────────────────────────────────────────────
dmv_sf_clean <- dmv_sf %>% filter(!is.na(asthma_prev))

tmap_mode("plot")
tm_shape(dmv_sf_clean) +
  tm_polygons(
    "asthma_prev",
    palette      = "Reds",
    style        = "quantile",
    border.alpha = 0.4,
    title        = "Asthma Prev (%)"
  ) +
  tm_layout(title = "Asthma Prevalence in DMV Tracts")

# ───────────────────────────────────────────────────────────────────────────────
# 5. Build Queen contiguity neighbors & spatial weights
# ───────────────────────────────────────────────────────────────────────────────
nb <- poly2nb(dmv_sf_clean, queen = TRUE)
lw <- nb2listw(nb, style = "W", zero.policy = TRUE)

# 6. Plot tract boundaries + neighbor links
plot(
  st_geometry(dmv_sf_clean),
  border = "lightgrey", reset = FALSE,
  main   = "DMV Tracts & Queen Neighbors"
)
centroids <- st_coordinates(st_centroid(dmv_sf_clean))
plot(nb, coords = centroids, add = TRUE, col = "blue")

# Step 2: Spatial Autocorrelation

# Global Moran’s I test for asthma prevalence
moran_res <- spdep::moran.test(
  dmv_sf_clean$asthma_prev,
  lw,
  zero.policy = TRUE
)
print(moran_res)

# Moran scatterplot
spdep::moran.plot(
  dmv_sf_clean$asthma_prev,
  lw,
  zero.policy = TRUE,
  labels = FALSE,
  pch    = 16,
  main   = "Moran Scatterplot: Asthma Prevalence"
)

# Interpretation:
# - Look at Moran’s I statistic and its p-value in moran_res.
# - A significantly positive Moran’s I (p < 0.05) indicates spatial clustering 
#   of high (or low) asthma prevalence, justifying spatial regression models.

## Step 3 Variable selection seeing vars_final_selection.Rmd
# Data of selected variables already loaded in Step 1.
# <!-- #Step 3: variables choosing -->
#   <!-- ```{r} -->
#   <!-- # ---- 0.  Packages ------------------------------------------------------------ -->
#   <!-- library(dplyr) -->
#   <!-- library(tidyr) -->
#   <!-- library(ggplot2) -->
#   <!-- library(broom)          # tidy() for model output -->
# <!-- library(psych)          # describe(), corr.test() -->
# <!-- library(ggcorrplot)     # prettier correlation heat-map -->
# 
# <!-- # ----------------------------------------------------------------- -->
#   <!-- # 0) Define variable groups for clarity & easy sub-setting -->
#   <!-- # ----------------------------------------------------------------- -->
#   
#   <!-- ## 0a. Pollution & physical environment -->
#   <!-- vars_pollute <- c("pm2.5", "ozone", "diesel", "road_proximity", -->
#                            <!--                   "percent_in_flood_zone")      # flood exposure -->
# 
# <!-- ## 0b. Socio-economic status (SES & financial stress) -->
#   <!-- vars_ses <- c("housing_cost_burden", "poverty", "unemployment") -->
#   
#   <!-- ## 0c. Education & language barriers -->
#   <!-- vars_edu_lang <- c("no_highschool", "limited_english") -->
#   
#   <!-- ## 0d. Household crowding & caregiving -->
#   <!-- vars_household <- c("crowded_housing", "single_parent") -->
#   
#   <!-- ## 0e. Age structure -->
#   <!-- vars_age <- c("age_under_17", "age_over_65") -->
#   
#   <!-- ## 0f. Racial/ethnic composition -->
#   <!-- vars_race <- c("minority")   # can add more like 'black', 'hispanic' if needed later -->
# 
# <!-- ## 0g. Outcome -->
#   <!-- var_outcome <- "asthma_prev" -->
#   
#   
#   <!-- # --------------------------------------------------------------- -->
#   <!-- # Combine everything into ONE master vector for the model step -->
#   <!-- # --------------------------------------------------------------- -->
#   <!-- vars_all <- c(var_outcome, -->
#                        <!--               vars_pollute, -->
#                        <!--               vars_ses, -->
#                        <!--               vars_edu_lang, -->
#                        <!--               vars_household, -->
#                        <!--               vars_age, -->
#                        <!--               vars_race) -->
#   <!-- # --------------------------------------------------------------- -->
#   <!-- # 1) Subset and drop rows with any missing values in vars_all -->
#   <!-- # --------------------------------------------------------------- -->
#   <!-- model_df <- dmv_data %>% -->
#   <!--   dplyr::select(all_of(vars_all)) %>% -->
#   <!--   tidyr::drop_na()          # geometry stays intact if dmv_data is an sf object -->
# 
# <!-- ``` -->
#   
#   
#   <!-- ```{r} -->
#   <!-- ## 2a. Spearman correlations with asthma_prev ------------------------------- -->
#   <!-- cont_vars  <- setdiff(names(model_df), "asthma_prev") -->
#   
#   <!-- cor_out <- psych::corr.test(model_df[, c("asthma_prev", cont_vars)], -->
#                                      <!--                              method = "spearman", adjust = "BH") -->
#   
#   <!-- # neat heat-map (upper triangle) ------------------------------------------- -->
#   <!-- ggcorrplot(cor_out$r, -->
#                     <!--            hc.order = TRUE,            # cluster similar variables -->
#                   <!--            type      = "upper", -->
#                     <!--            lab       = TRUE, -->
#                     <!--            title     = "Spearman correlations with asthma prevalence") -->
#   
#   
#   <!-- # ---- Packages --------------------------------------------------------------- -->
#   <!-- if (!require(car)) install.packages("car") -->
#   <!-- library(car)          # for vif() -->
# 
# <!-- # ---- Trimmed-variable linear model ----------------------------------------- -->
#   <!-- lm_trim <- lm( -->
#                         <!--   asthma_prev ~ pm2.5 + ozone + diesel + road_proximity +  -->
#                         <!--                 percent_in_flood_zone + -->
#                         <!--                 housing_cost_burden + poverty + unemployment + -->
#                         <!--                 no_highschool + limited_english + -->
#                         <!--                 crowded_housing + single_parent + -->
#                         <!--                 age_under_17 + age_over_65, -->
#                         <!--   data = dmv_data -->
#                         <!-- ) -->
#   
#   <!-- # ---- Re-estimate VIFs ------------------------------------------------------- -->
#   <!-- vif_vals <- vif(lm_trim) -->
#   <!-- print(vif_vals) -->
#   
#   <!-- # optional: flag any values > 4 -->
#   <!-- vif_vals[vif_vals > 4] -->
#   
#   
#   <!-- ``` -->
  
  
  
  
  
# Step 4: Linear Regression Model

library(dplyr)
library(spdep)
library(tidyr)

# -------------------------------------------------------------------
# 1) Define variables for the final OLS / spatial-diagnostic workflow
# -------------------------------------------------------------------

vars_selected <- c("ozone", "cancer_prob", "watershed_proximity", "area_sqmi", "house_age", "walkability",
                   "percent_in_flood_zone", "asian", "hispanic", "two_or_more", "limited_english",
                   "uninsured", "unemployment", "labor_participation", "single_parent", "mobile_homes", "housing_stress")


# -------------------------------------------------------------------
# 2) Keep only tracts with complete data on vars
# -------------------------------------------------------------------
model_data <- dmv_sf_clean %>%
  dplyr::select(asthma_prev, all_of(vars_selected)) %>%
  tidyr::drop_na()


# -------------------------------------------------------------------
# 3) Build neighbors & spatial weights on the SAME subset
# -------------------------------------------------------------------
nb_mod <- spdep::poly2nb(model_data, queen = TRUE)
lw_mod <- spdep::nb2listw(nb_mod, style = "W", zero.policy = TRUE)

library(sf)

# 4) Fit OLS without geometry
ols_model <- lm(
  asthma_prev ~ .,
  data = model_data |> st_drop_geometry()   # <- remove spatial column
)
summary(ols_model)
# -------------------------------------------------------------------
# 5) Attach residuals to the sf data frame
# -------------------------------------------------------------------
model_data$resid_ols <- residuals(ols_model)

# -------------------------------------------------------------------
# 6) Global Moran’s I test on the OLS residuals
# -------------------------------------------------------------------
resid_moran <- spdep::moran.test(
  model_data$resid_ols,
  lw_mod,
  zero.policy = TRUE
)
print(resid_moran)

# -------------------------------------------------------------------
# 7) Moran scatterplot of OLS residuals
# -------------------------------------------------------------------
spdep::moran.plot(
  model_data$resid_ols,
  lw_mod,
  zero.policy = TRUE,
  labels = FALSE,
  pch    = 16,
  main   = "Moran Scatterplot: OLS Residuals"
)

#step 5 compare sdem sem car 

library(sf)
library(dplyr)
library(tidyr)
library(spdep)       # neighbours + weights
library(spatialreg)  # spautolm(), errorsarlm(), lagsarlm()

## ───────────────────────────────────────────────────────────────
## 0.  Variable list stays in ONE place
## ───────────────────────────────────────────────────────────────
vars_selected <- c(
  "ozone", "cancer_prob", "watershed_proximity", "area_sqmi",
  "house_age", "walkability", "percent_in_flood_zone",
  "asian", "hispanic", "two_or_more", "limited_english",
  "uninsured", "unemployment", "labor_participation",
  "single_parent", "mobile_homes", "housing_stress"
)

## ───────────────────────────────────────────────────────────────
## 1.  Build analysis set once
## ───────────────────────────────────────────────────────────────
sf_model <- dmv_sf_clean |>
  select(asthma_prev, all_of(vars_selected)) |>
  drop_na()                       # still an sf object

model_data <- st_drop_geometry(sf_model)     # plain data.frame

nb_mod  <- poly2nb(sf_model, queen = TRUE)   # contiguity neighbours
lw_mod  <- nb2listw(nb_mod, style = "W", zero.policy = TRUE)

## common formula
form <- as.formula(
  paste("asthma_prev ~", paste(vars_selected, collapse = " + "))
)

## ───────────────────────────────────────────────────────────────
## 2.  OLS (already fitted earlier, re-fit here for completeness)
## ───────────────────────────────────────────────────────────────
ols_model <- lm(form, data = model_data)

## ───────────────────────────────────────────────────────────────
## 3.  CAR model (Gaussian)
## ───────────────────────────────────────────────────────────────
car_model <- spautolm(
  form, data = model_data,
  listw = lw_mod,
  family = "CAR",
  zero.policy = TRUE
)
summary(car_model)

## ───────────────────────────────────────────────────────────────
## 4.  SEM  vs  SDEM
## ───────────────────────────────────────────────────────────────
sem_model <- errorsarlm(
  form, data = model_data,
  listw = lw_mod,
  Durbin = FALSE,
  zero.policy = TRUE
)

sdem_model <- errorsarlm(
  form, data = model_data,
  listw = lw_mod,
  Durbin = TRUE,        # adds spatially-lagged X’s
  zero.policy = TRUE
)

## ───────────────────────────────────────────────────────────────
## 5.  Information‐criteria comparison
## ───────────────────────────────────────────────────────────────
AICvals <- c(
  OLS = AIC(ols_model),
  SEM = AIC(sem_model),
  SDEM = AIC(sdem_model),
  CAR = AIC(car_model)
)
print(round(AICvals, 2))

## ───────────────────────────────────────────────────────────────
## 6.  Pseudo-R² & Moran’s I
## ───────────────────────────────────────────────────────────────
pseudoR2 <- function(model, y)
  1 - sum(residuals(model)^2) / sum((y - mean(y))^2)

cat("Pseudo-R²\n",
    "  OLS :", pseudoR2(ols_model,  model_data$asthma_prev), "\n",
    "  SEM :", pseudoR2(sem_model,  model_data$asthma_prev), "\n",
    "  SDEM:", pseudoR2(sdem_model, model_data$asthma_prev), "\n",
    "  CAR :", pseudoR2(car_model,  model_data$asthma_prev), "\n")

moran_car <- moran.test(residuals(car_model), lw_mod, zero.policy = TRUE)
cat("Moran p-value (CAR residuals):", moran_car$p.value, "\n")

## ───────────────────────────────────────────────────────────────
## 7.  LR‐test  and  Hausman test (SDEM vs SEM)
## ───────────────────────────────────────────────────────────────
lr_res <- anova(sdem_model, sem_model)       # LR test
print(lr_res)

haus_res <- spatialreg::Hausman.test(sdem_model, sem_model)
print(haus_res)

## safer extraction
lr_p <- as.numeric(as.character(lr_res[2, "Pr(>Chi)"]))  # second row = full SDEM vs SEM
haus_p <- as.numeric(haus_res$p.value)

comp <- data.frame(
  Model     = c("SEM", "SDEM"),
  AIC       = c(AIC(sem_model),  AIC(sdem_model)),
  LR_p      = c(NA, round(lr_p,   4)),
  Hausman_p = c(NA, round(haus_p, 4))
)
print(comp, row.names = FALSE)

#step6 visualization
# ───────────────────────────────────────────────────────────
#  Libraries
# ───────────────────────────────────────────────────────────
library(sf)
library(dplyr)
library(tidyr)
library(spdep)        # neighbours, weights
library(spatialreg)   # spatial models
library(Matrix)       # pseudo-inverse (Hausman fallback)
library(tmap)         # mapping
library(broom)        # tidy coefficients
library(ggplot2)

#  0.  Master predictor list
# ───────────────────────────────────────────────────────────
vars_selected <- c(
  "ozone", "cancer_prob", "watershed_proximity", "area_sqmi",
  "house_age", "walkability", "percent_in_flood_zone",
  "asian", "hispanic", "two_or_more", "limited_english",
  "uninsured", "unemployment", "labor_participation",
  "single_parent", "mobile_homes", "housing_stress"
)

# ───────────────────────────────────────────────────────────
#  1.  Build modelling data
# ───────────────────────────────────────────────────────────
sf_model <- dmv_sf_clean |>
  select(asthma_prev, all_of(vars_selected)) |>
  drop_na()

# 1.1  Drop pure islands (no neighbours) so all rows have at least 1 nb
nb_all  <- poly2nb(sf_model, queen = TRUE)
is_isle <- lengths(nb_all) == 0
if (any(is_isle)) {
  message("Dropping ", sum(is_isle), " island tracts with no neighbours.")
  sf_model <- sf_model[!is_isle, ]
}

model_df <- st_drop_geometry(sf_model)

# 1.2  Rebuild neighbour list & weights
nb_mod <- poly2nb(sf_model, queen = TRUE, snap = 1e-7)   # small snap for tiny gaps
lw_mod <- nb2listw(nb_mod, style = "W", zero.policy = TRUE)

# 1.3  Symmetric weights for CAR
lw_sym <- listw2U(lw_mod)   # binary, symmetric

# ───────────────────────────────────────────────────────────
#  2.  Unified formula
# ───────────────────────────────────────────────────────────
form <- reformulate(vars_selected, response = "asthma_prev")

# ───────────────────────────────────────────────────────────
#  3.  Fit models
# ───────────────────────────────────────────────────────────
ols_model  <- lm(form, data = model_df)

sem_model  <- errorsarlm(form, data = model_df,
                         listw = lw_mod, Durbin = FALSE,
                         zero.policy = TRUE)

sdem_model <- errorsarlm(form, data = model_df,
                         listw = lw_mod, Durbin = TRUE,
                         zero.policy = TRUE)

car_model  <- spautolm(form, data = model_df,
                       listw = lw_sym, family = "CAR",
                       zero.policy = TRUE)

# ───────────────────────────────────────────────────────────
#  4.  LR + Hausman tests  (SDEM vs SEM)
# ───────────────────────────────────────────────────────────
lr_res   <- anova(sdem_model, sem_model)
lr_p     <- as.numeric(as.character(lr_res[2, "Pr(>Chi)"]))  # coerce factor→numeric

haus_res <- spatialreg::Hausman.test(sdem_model, sem_model)
haus_p   <- haus_res$p.value

# ───────────────────────────────────────────────────────────
#  5.  Model-comparison table
# ───────────────────────────────────────────────────────────
aic_tbl <- data.frame(
  Model  = c("OLS", "SEM", "SDEM", "CAR"),
  AIC    = c(AIC(ols_model),  AIC(sem_model),
             AIC(sdem_model), AIC(car_model)),
  LogLik = c(logLik(ols_model),  logLik(sem_model),
             logLik(sdem_model), logLik(car_model))
) |>
  arrange(AIC)

print(aic_tbl, row.names = FALSE)
cat("\nLR test (SDEM vs SEM):   p =", round(lr_p,   4),
    "\nHausman p-value:          ", round(haus_p, 4), "\n")

# ───────────────────────────────────────────────────────────
#  6.  Append SEM predictions & residuals for mapping
# ───────────────────────────────────────────────────────────
sf_model <- sf_model |>
  mutate(pred_sem  = predict(sem_model),
         resid_sem = residuals(sem_model))

# ───────────────────────────────────────────────────────────
#  7.  Quick maps (tmap, legacy syntax)
# ───────────────────────────────────────────────────────────
tmap_arrange(
  tm_shape(sf_model) +
    tm_polygons("asthma_prev", palette = "Reds",
                title = "Observed Asthma") +
    tm_layout(title = "Observed Asthma"),
  
  tm_shape(sf_model) +
    tm_polygons("pred_sem", palette = "Blues",
                title = "Predicted (SEM)") +
    tm_layout(title = "Predicted (SEM)"),
  ncol = 2
)

tm_shape(sf_model) +
  tm_polygons("resid_sem",
              palette = "brewer.rd_bu",    # explicit palette
              style   = "quantile",
              title   = "SEM Residuals") +
  tm_layout(title = "Residuals (SEM)")

# ───────────────────────────────────────────────────────────
#  8.  ggplot summaries
# ───────────────────────────────────────────────────────────
coef_df <- tidy(sdem_model, conf.int = TRUE)

ggplot(coef_df, aes(estimate, term)) +
  geom_pointrange(aes(xmin = conf.low, xmax = conf.high)) +
  geom_vline(xintercept = 0, linetype = "dotted") +
  labs(x = "Estimate (95 % CI)", y = NULL,
       title = "SDEM Coefficients") +
  theme_bw()

ggplot(aic_tbl, aes(reorder(Model, AIC), AIC)) +
  geom_col(fill = "steelblue") +
  geom_text(aes(label = round(AIC, 1)), vjust = -0.3) +
  labs(y = "AIC", x = NULL,
       title = "Model Fit Comparison (lower is better)") +
  theme_minimal()
