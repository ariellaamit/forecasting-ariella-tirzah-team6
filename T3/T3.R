library(tidyverse)
library(sf)
library(DBI)
library(here)
library(leaflet)
library(maptiles)
library(tidyterra)
library(knitr)
library(tigris)
library(omxr)
library(scales)
library(cowplot)
library(RColorBrewer)
library(chorddiag)
library(units)

sf::sf_use_s2(FALSE)

base_pa <- here("C:/model/outputs/Base/_demand/td/base_matrix.omx") |>
  read_all_omx(c("hb", "nhb")) |>
  mutate(total_hh_trips = hb + nhb)

head(base_pa) |>
  kable(digits = 2)

recession_pa <- here("C:/model/outputs/recession/_demand/td/recession_matrix.omx") |>
  read_all_omx(c("hb", "nhb")) |>
  mutate(total_hh_trips = hb + nhb)

head(recession_pa) |>
  kable(digits = 2)

TAZs <- here("C:/model/inputs/zonal/shp/CTPS_TDM23_TAZ_2017g_v202303.shp") |>
  st_read(quiet = TRUE) |>
  st_transform("WGS84")

leaflet(TAZs) |>
  addProviderTiles(provider = "CartoDB.Positron") |>
  addPolygons(weight = 2,
              color = "blue",
              fillColor = "blue",
              fillOpacity = 0.1,
              highlightOptions = highlightOptions(weight = 3,
                                                  fillOpacity = 0.5),
              label = TAZs$taz_id)

TAZs <- TAZs |>
  filter(state == "MA",
         mpo != "BRPC",
         mpo != "FRCOG")

base_pa <- base_pa |>
  filter(origin %in% TAZs$taz_id,
         destination %in% TAZs$taz_id)

recession_pa <- recession_pa |>
  filter(origin %in% TAZs$taz_id,
         destination %in% TAZs$taz_id)

leaflet(TAZs) |>
  addProviderTiles(provider = "CartoDB.Positron") |>
  addPolygons(weight = 2,
              color = "blue",
              fillColor = "blue",
              fillOpacity = 0.1,
              highlightOptions = highlightOptions(weight = 3,
                                                  fillOpacity = 0.5),
              label = TAZs$taz_id)

base_productions <- base_pa |>
  group_by(origin) |>
  summarize(productions = sum(total_hh_trips)) |>
  rename(taz_id = origin)

base_attractions <- base_pa |>
  group_by(destination) |>
  summarize(attractions = sum(total_hh_trips)) |>
  rename(taz_id = destination)

base_intrazonal <- base_pa |>
  filter(origin == destination) |>
  select(origin, total_hh_trips) |>
  rename(taz_id = origin,
         intrazonal = total_hh_trips) |>
  full_join(base_productions) |>
  full_join(base_attractions) |>
  filter(productions + attractions > 0) |>
  mutate(pct_intra_attr = intrazonal / attractions,
         pct_intra_prod = intrazonal / productions) 

base_intrazonal <- TAZs |>
  inner_join(base_intrazonal)

base_map <- get_tiles(base_intrazonal,
                      provider = "CartoDB.Positron",
                      zoom = 8,
                      crop = TRUE)

ggplot(base_intrazonal) +
  geom_spatraster_rgb(data = base_map) +
  geom_sf(color = NA,
          alpha = 0.7,
          aes(fill = intrazonal)) +
  scale_fill_viridis_c(transform = "log",
                       breaks = breaks <- 10^seq(-3, 4, by=1),
                       labels = c(formatC(breaks[1:3], 
                                          format = "f",
                                          digits = 3),
                                  formatC(breaks[4:8],
                                          format = "d",
                                          big.mark = " ")),
                       name = "Number of\nintrazonal trips") +
  theme_void()

ggplot(base_intrazonal) +
  geom_spatraster_rgb(data = base_map) +
  geom_sf(color = NA,
          alpha = 0.7,
          aes(fill = pct_intra_prod)) +
  scale_fill_viridis_c(name = "Intrazonal trips\n(% of productions)",
                       breaks = breaks <- seq(0, 0.7, by=0.1),
                       labels = paste0(round(breaks*100),"%")) +
  theme_void()

ggplot(base_intrazonal) +
  geom_spatraster_rgb(data = base_map) +
  geom_sf(color = NA,
          alpha = 0.7,
          aes(fill = pct_intra_attr)) +
  scale_fill_viridis_c(name = "Intrazonal trips\n(% of attractions)",
                       breaks = breaks <- seq(0, 0.9, by=0.1),
                       labels = paste0(round(breaks*100),"%")) +
  theme_void()

#### Recession

recession_productions <- recession_pa |>
  group_by(origin) |>
  summarize(productions = sum(total_hh_trips)) |>
  rename(taz_id = origin)

recession_attractions <- recession_pa |>
  group_by(destination) |>
  summarize(attractions = sum(total_hh_trips)) |>
  rename(taz_id = destination)

recession_intrazonal <- recession_pa |>
  filter(origin == destination) |>
  select(origin, total_hh_trips) |>
  rename(taz_id = origin,
         intrazonal = total_hh_trips) |>
  full_join(recession_productions) |>
  full_join(recession_attractions) |>
  filter(productions + attractions > 0) |>
  mutate(pct_intra_attr = intrazonal / attractions,
         pct_intra_prod = intrazonal / productions) 

recession_intrazonal <- TAZs |>
  inner_join(recession_intrazonal)

recession_map <- get_tiles(recession_intrazonal,
                      provider = "CartoDB.Positron",
                      zoom = 8,
                      crop = TRUE)

ggplot(recession_intrazonal) +
  geom_spatraster_rgb(data = recession_map) +
  geom_sf(color = NA,
          alpha = 0.7,
          aes(fill = intrazonal)) +
  scale_fill_viridis_c(transform = "log",
                       breaks = breaks <- 10^seq(-3, 4, by=1),
                       labels = c(formatC(breaks[1:3], 
                                          format = "f",
                                          digits = 3),
                                  formatC(breaks[4:8],
                                          format = "d",
                                          big.mark = " ")),
                       name = "Number of\nintrazonal trips") +
  theme_void()

ggplot(recession_intrazonal) +
  geom_spatraster_rgb(data = recession_map) +
  geom_sf(color = NA,
          alpha = 0.7,
          aes(fill = pct_intra_prod)) +
  scale_fill_viridis_c(name = "Intrazonal trips\n(% of productions)",
                       breaks = breaks <- seq(0, 0.7, by=0.1),
                       labels = paste0(round(breaks*100),"%")) +
  theme_void()

ggplot(recession_intrazonal) +
  geom_spatraster_rgb(data = recession_map) +
  geom_sf(color = NA,
          alpha = 0.7,
          aes(fill = pct_intra_attr)) +
  scale_fill_viridis_c(name = "Intrazonal trips\n(% of attractions)",
                       breaks = breaks <- seq(0, 0.9, by=0.1),
                       labels = paste0(round(breaks*100),"%")) +
  theme_void()

## desire lines

TAZ_pts <- st_centroid(TAZs)

make_desire_lines <- function(od_df,
                              points,
                              origin_column,
                              destination_column,
                              trips_column,
                              taz_id_column) {
  
  od_df <- od_df |>
    ungroup() |>
    rename(origin = all_of(origin_column),
           destination = all_of(destination_column),
           trips = all_of(trips_column)) |>
    select(origin, destination, trips) |>
    filter(origin != destination)
  
  points <- points |>
    rename(taz_id = all_of(taz_id_column))
  
  desire_line_data <- od_df |>
    mutate(id = seq(1, nrow(od_df))) |>
    pivot_longer(cols = c(origin, destination),
                 names_to = "o_d",
                 values_to = "taz_id") 
  
  desire_lines <- points |>
    right_join(desire_line_data) |>
    group_by(id) |>
    summarize(trips = mean(trips),
              origin_taz = first(taz_id),
              destin_taz = last(taz_id)) |>
    ungroup() |>
    select(origin_taz, destin_taz, trips) |>
    st_cast("LINESTRING")
  
  desire_lines
  
}

#### Fewer towns

boston_town_pts <- TAZ_pts |>
  filter(mpo == "BRMPO") |>
  group_by(town) |>
  summarise(n_zones = n()) |>
  st_centroid()

TAZ_town_codes <- st_drop_geometry(TAZ_pts) |>
  filter(mpo == "BRMPO") |>
  select(taz_id, town) 

base_pa_towns <- base_pa |>
  rename(taz_id = origin) |>
  left_join(TAZ_town_codes) |>
  rename(origin_town = town,
         origin = taz_id) |>
  rename(taz_id = destination) |>
  left_join(TAZ_town_codes) |>
  rename(destin_town = town,
         destination = taz_id) |>
  group_by(origin_town, destin_town) |>
  summarize(trips = sum(total_hh_trips)) |>
  filter(!is.na(origin_town),
         !is.na(destin_town))

fewer_towns <- c("CAMBRIDGE",
                 "BOSTON",
                 "FRAMINGHAM",
                 "GLOUCESTER",
                 "BROOKLINE",
                 "QUINCY",
                 "LYNN")

recession_fewer_pa_towns <- recession_pa_towns |>
  filter(origin_town %in% fewer_towns,
         destin_town %in% fewer_towns)

base_fewer_pa_towns <- base_pa_towns |>
  filter(origin_town %in% fewer_towns,
         destin_town %in% fewer_towns)

desire_fewer_base_towns <- make_desire_lines(od_df = base_fewer_pa_towns,
                                             points = boston_town_pts,
                                             origin_column = "origin_town",
                                             destination_column = "destin_town",
                                             trips_column = "trips",
                                             taz_id_column = "town")
##### Aggregate MPOs

mpo_pts <- TAZ_pts |>
  group_by(mpo) |>
  summarise(n_zones = n()) |>
  st_centroid()

TAZ_mpo_codes <- st_drop_geometry(TAZ_pts) |>
  select(taz_id, mpo) 

recession_pa_mpos <- recession_pa |>
  rename(taz_id = origin) |>
  left_join(TAZ_mpo_codes) |>
  rename(origin_mpo = mpo,
         origin = taz_id) |>
  rename(taz_id = destination) |>
  left_join(TAZ_mpo_codes) |>
  rename(destin_mpo = mpo,
         destination = taz_id) |>
  group_by(origin_mpo, destin_mpo) |>
  summarize(trips = sum(total_hh_trips)) |>
  filter(!is.na(origin_mpo),
         !is.na(destin_mpo))


desire_recession_mpos <- make_desire_lines(od_df = recession_pa_mpos,
                                      points = mpo_pts,
                                      origin_column = "origin_mpo",
                                      destination_column = "destin_mpo",
                                      trips_column = "trips",
                                      taz_id_column = "mpo")

recession_map <- get_tiles(desire_recession_mpos,
                      provider = "CartoDB.Positron",
                      zoom = 8,
                      crop = TRUE)

ggplot(desire_recession_mpos) +
  geom_spatraster_rgb(data = recession_map) +
  geom_sf(aes(linewidth = trips),
          alpha = 0.5,
          color = "olivedrab3") +
  theme_void()




#### test

mpo_pts <- TAZ_pts |>
  group_by(mpo) |>
  summarise(n_zones = n()) |>
  st_centroid()

TAZ_mpo_codes <- st_drop_geometry(TAZ_pts) |>
  select(taz_id, mpo) 

base_pa_mpos <- base_pa |>
  rename(taz_id = origin) |>
  left_join(TAZ_mpo_codes) |>
  rename(origin_mpo = mpo,
         origin = taz_id) |>
  rename(taz_id = destination) |>
  left_join(TAZ_mpo_codes) |>
  rename(destin_mpo = mpo,
         destination = taz_id) |>
  group_by(origin_mpo, destin_mpo) |>
  summarize(trips = sum(total_hh_trips)) |>
  filter(!is.na(origin_mpo),
         !is.na(destin_mpo))

desire_base_mpos <- make_desire_lines(od_df = base_pa_mpos,
                                      points = mpo_pts,
                                      origin_column = "origin_mpo",
                                      destination_column = "destin_mpo",
                                      trips_column = "trips",
                                      taz_id_column = "mpo")

base_map <- get_tiles(desire_base_mpos,
                      provider = "CartoDB.Positron",
                      zoom = 8,
                      crop = TRUE)

ggplot(desire_base_mpos) +
  geom_spatraster_rgb(data = base_map) +
  geom_sf(aes(linewidth = trips),
          alpha = 0.5,
          color = "olivedrab3") +
  theme_void()

#### chord

mat <- matrix(recession_fewer_pa_towns$trips, 
              nrow = 7, 
              ncol = 7, 
              byrow = TRUE) 

chorddiag(mat, groupNames = recession_fewer_pa_towns$destin_town[1:7])

mat_base <- matrix(base_fewer_pa_towns$trips, 
              nrow = 7, 
              ncol = 7, 
              byrow = TRUE) 

chorddiag(mat_base, groupNames = base_fewer_pa_towns$destin_town[1:7])

####


base_pa <- base_pa |>
  mutate(total_hh_trips_base = hb + nhb) |>
  select(origin, destination, total_hh_trips_base)

recession_pa <- recession_pa |>
  mutate(total_hh_trips_recession = hb + nhb) |>
  select(origin, destination, total_hh_trips_recession)

tolerance <- 50

comparison <- inner_join(base_pa, recession_pa) |>
  mutate(difference = total_hh_trips_recession - total_hh_trips_base) |>
  filter(abs(difference) > tolerance)

head(comparison) |>
  kable(digits = 1)

###### Where are those TAZs located
changed_TAZ_list <- unique(c(comparison$origin, comparison$destination))

lost_p <- comparison |>
  filter(difference < 0) |>
  group_by(origin) |>
  summarise(lost_p = -1 * sum(difference)) |>
  rename(taz_id = origin)

lost_a <- comparison |>
  filter(difference < 0) |>
  group_by(destination) |>
  summarise(lost_a = -1 * sum(difference)) |>
  rename(taz_id = destination)

gain_p <- comparison |>
  filter(difference > 0) |>
  group_by(origin) |>
  summarise(gain_p = sum(difference)) |>
  rename(taz_id = origin)

gain_a <- comparison |>
  filter(difference > 0) |>
  group_by(destination) |>
  summarise(gain_a = sum(difference)) |>
  rename(taz_id = destination)

comparison_summary <- full_join(gain_a,
                                gain_p) |>
  full_join(lost_a) |>
  full_join(lost_p) |>
  replace_na(list(lost_a = 0,
                  lost_p = 0, 
                  gain_a = 0,
                  gain_p = 0))

changed_TAZs <- TAZs |>
  right_join(comparison_summary)

change_labels <- paste0("Gained ",
                        round(changed_TAZs$gain_a),
                        " trips to here<br/>",
                        "Lost ",
                        round(changed_TAZs$lost_a),
                        " trips to here<br/>",
                        "Gained ",
                        round(changed_TAZs$gain_p),
                        " trips from here<br/>",
                        "Lost ",
                        round(changed_TAZs$lost_p),
                        " trips from here<br/>") |>
  lapply(htmltools::HTML)

leaflet(changed_TAZs) |>
  addProviderTiles(provider = "CartoDB.Positron") |>
  addPolygons(weight = 2,
              color = "olivedrab",
              fillColor = "olivedrab",
              fillOpacity = 0.1,
              highlightOptions = highlightOptions(weight = 3,
                                                  fillOpacity = 0.5),
              label = change_labels)


intrazonal_trips <- comparison |>
  filter(origin == destination) |>
  rename(taz_id = origin) |>
  select(-destination)

interzonal_trips <- comparison |>
  filter(origin != destination) 

ggplot(intrazonal_trips) +
  geom_histogram(aes(x = difference),
                 binwidth = 100,
                 fill = "olivedrab",
                 alpha = 0.5,
                 color = "olivedrab") +
  scale_y_continuous(breaks = seq(0, 800, by=100),
                     name = "Number of zones (among zones with\na differnce of at least +/- 50 trips)") +
  scale_x_continuous(breaks = seq(-5000, 2500, by = 500),
                     name = "Difference in number of intrazonal trips") +
  theme_minimal()

#### spatial distribution 


intrazonal_locs <- changed_TAZs |>
  right_join(intrazonal_trips)

base_map <- get_tiles(intrazonal_locs,
                      provider = "CartoDB.Positron",
                      zoom = 13,
                      crop = TRUE)

ggplot(intrazonal_locs) +
  geom_spatraster_rgb(data = base_map) +
  geom_sf(color = "gray",
          alpha = 0.7,
          aes(fill = difference)) +
  scale_fill_viridis_c(transform = "log",
                       breaks = c(50*2^seq(1, 10, by=1)),
                       direction = -1,
                       name = "Increase in\nintrazonal\ntrips") +
  theme_void()


#### Desire lines

changed_TAZ_points <- changed_TAZs |>
  st_centroid() |>
  select(taz_id)

desire_line_data <- interzonal_trips |>
  mutate(id = seq(1, nrow(interzonal_trips))) |>
  pivot_longer(cols = c(origin, destination),
               names_to = "o_d",
               values_to = "taz_id") 

desire_line_change <- changed_TAZ_points |>
  right_join(desire_line_data) |>
  group_by(id) |>
  summarize(difference = mean(difference),
            origin_taz = first(taz_id),
            destin_taz = last(taz_id)) |>
  ungroup() |>
  select(difference, origin_taz, destin_taz) |>
  st_cast("LINESTRING")

base_map <- get_tiles(desire_line_change,
                      provider = "CartoDB.DarkMatter",
                      zoom = 12,
                      crop = TRUE)

ggplot(desire_line_change) + 
  geom_spatraster_rgb(data = base_map) +
  geom_sf(aes(color = difference)) +
  scale_color_gradient2(low = muted("red"), 
                        mid = "white",
                        high = muted("blue"),
                        midpoint = 0) +
  theme_void()

desire_line_gain <- desire_line_change |>
  filter(difference > 0)

desire_line_loss <- desire_line_change |>
  filter(difference < 0) |>
  mutate(difference = -1 * difference)

base_map <- get_tiles(desire_line_change,
                      provider = "CartoDB.Positron",
                      zoom = 12,
                      crop = TRUE)

ggplot() + 
  geom_spatraster_rgb(data = base_map) +
  geom_sf(data = desire_line_loss,
          alpha = 0.2,
          aes(linewidth = difference,
              color = "Reduced demand")) +
  geom_sf(data = desire_line_gain,
          alpha = 0.2,
          aes(linewidth = difference,
              color = "Increased demand")) +
  scale_linewidth(name = "Magnitude of difference\n(number of trips)") +
  scale_color_manual(name = "Direction of difference",
                     values = c(muted("blue"), muted("red"))) +
  guides(color = guide_legend(override.aes = list(linewidth = 2,
                                                  alpha = 0.5))) +
  theme_void()

gain_map <- ggplot() + 
  geom_spatraster_rgb(data = base_map) +
  geom_sf(data = desire_line_gain,
          alpha = 0.15,
          color = "orange",
          aes(linewidth = difference)) +
  scale_linewidth(name = "Magnitude of difference",
                  limits = c(0,500),
                  breaks = breaks <- seq(100, 500, by = 100),
                  labels = paste0(breaks, " trips")) +
  theme_void() 

loss_map <- ggplot() + 
  geom_spatraster_rgb(data = base_map) +
  geom_sf(data = desire_line_loss,
          alpha = 0.15,
          color = "orange",
          aes(linewidth = difference)) +
  scale_linewidth(name = "Magnitude of difference",
                  limits = c(0,500),
                  breaks = breaks <- seq(100, 500, by = 100),
                  labels = paste0(breaks, " trips")) +
  theme_void() 

legend <- get_legend(loss_map)

plot_grid(gain_map + theme(legend.position = "none"), 
          loss_map + theme(legend.position = "none"), 
          legend,
          nrow = 1,
          labels = c("Trip increases",
                     "Trip decreases",
                     ""),
          label_size = 10,
          label_y = 0.8,
          label_x = -0.12)