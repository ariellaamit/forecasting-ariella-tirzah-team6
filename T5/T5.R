library(tidyverse)
library(sf)
library(here)
library(maptiles)
library(tidyterra)
library(knitr)
library(tigris)

##### Load highway network
all_roads <- here("C:/model/inputs/networks/tdm23.1.0/2019/hwy.geojson") |>
  st_read()

##### Make map
ggplot(all_roads) +
  geom_sf() +
  theme_void()

##### Load link volumes
am_hwy_flows_base <- here("C:/model/outputs/Base/_assignment/flows_am.csv") |>
  read_csv() |>
  rename(ID = ID1,
         base_flow = Tot_Flow_PCE) |>
  select(ID, base_flow)

am_hwy_flows_alt <- here("C:/model/outputs/recession/_assignment/flows_am.csv") |>
  read_csv() |>
  rename(ID = ID1,
         alt_flow = Tot_Flow_PCE) |>
  select(ID, alt_flow)

#####
link_vols_compare <- inner_join(all_roads, am_hwy_flows_base) |>
  left_join(am_hwy_flows_alt) |>
  replace_na(list(alt_flow = 0)) |>
  mutate(flow_diff = alt_flow - base_flow) 

missing_bridge_vicinity <- tibble(lat = 42.400187426238766, 
                                  lon = -71.08355937577717) |>
  st_as_sf(coords = c("lon", "lat"), crs = "WGS84") |>
  st_transform(26986) |>
  st_buffer(dist = 5000)

vicinity_links <- link_vols_compare |>
  st_transform(26986) |>
  st_filter(missing_bridge_vicinity)

vicinity_increases <- vicinity_links |>
  filter(flow_diff > 50)

vicinity_decreases <- vicinity_links |>
  filter(flow_diff < -50) |>
  mutate(flow_diff = -1 * flow_diff)

base_map <- get_tiles(vicinity_links,
                      provider = "CartoDB.Positron")

ggplot(vicinity_increases) +
  geom_spatraster_rgb(data =base_map) +
  geom_sf(aes(color = flow_diff),
          linewidth = 1) +
  scale_color_viridis_c(transform = "log2",
                        breaks = c(80, 160, 320, 640, 1280),
                        direction = -1,
                        name = "Increase in morning\npeak traffic volumes") +
  theme_void()

ggplot(vicinity_decreases) +
  geom_spatraster_rgb(data =base_map) +
  geom_sf(aes(color = flow_diff),
          linewidth = 1) +
  scale_color_viridis_c(transform = "log2",
                        breaks = c(80, 160, 320, 640, 1280, 2560, 5120),
                        direction = -1,
                        name = "Decrease in morning\npeak traffic volumes") +
  theme_void()

##### Load transit stop 
stop_locs <- here("C:/model/inputs/networks/tdm23.1.0/2019/stops.geojson") |>
  st_read()

base_map <- get_tiles(stop_locs,
                      provider = "CartoDB.Positron",
                      zoom = 8,
                      crop = TRUE)

ggplot(stop_locs) +
  geom_spatraster_rgb(data = base_map) +
  geom_sf(size = 1, color = "orange") +
  theme_void()

##### Load boarding data

base_onoff <- here("C:/model/outputs/Base/_assignment/onoff_tw_am.csv") |>
  read_csv(show_col_types = FALSE) |>
  select(STOP, On, Off) |>
  rename(base_on = On,
         base_off = Off,
         ID = STOP)

alt_onoff <- here("C:/model/outputs/recession/_assignment/onoff_tw_am.csv") |>
  read_csv(show_col_types = FALSE) |>
  select(STOP, On, Off) |>
  rename(alt_on = On,
         alt_off = Off,
         ID = STOP)

#### Compare ridership 

stop_diff <- stop_locs |>
  inner_join(base_onoff) |>
  left_join(alt_onoff) |>
  mutate(base_total = base_on + base_off,
         alt_total = alt_on + alt_off,
         diff = alt_total - base_total) |>
  mutate(abs_diff = abs(diff)) |>
  arrange(-abs_diff) |>
  mutate(inc_dec = ifelse(diff < 0, "Decrease", "Increase"))

stop_diff |>
  st_drop_geometry() |>
  select(ID, diff) |>
  head(n = 20) |>
  kable()

big_stop_diffs <- stop_diff |>
  filter(abs_diff > 100)

base_map <- get_tiles(big_stop_diffs,
                      provider = "CartoDB.Positron",
                      zoom = 12,
                      crop = TRUE)

ggplot(big_stop_diffs) +
  geom_spatraster_rgb(data = base_map) +
  geom_sf(aes(size = abs_diff,
              color = inc_dec),
          alpha = 0.3) +
  scale_size(name = "Magnitude of change") +
  scale_color_manual(name = "Direction of change",
                     values = c("red", "blue"),
                     labels = c("Decrease",
                                "Increase")) +
  theme_void()