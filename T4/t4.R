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
library(RColorBrewer)

sf::sf_use_s2(FALSE)
  
#### Base Scenario

base_mc <- here("C:/model/outputs/Base/_demand/mc/mc_hhpurp_daily_omx.omx") |>
  read_all_omx()

head(base_mc) |>
  kable(digits = 1)

#### Alternative Scenario

alt_mc <- here("C:/model/outputs/recession/_demand/mc/mc_hhpurp_daily_omx.omx") |>
  read_all_omx()

head(alt_mc) |>
  kable(digits = 1)

#### Calculating mode shares and differences

mode_share_by_zone <- function(mc_table) {
  mc_by_p_zone <- mc_table |>
    group_by(origin) |>
    summarise(across(everything(), list(p = sum))) |>
    rename(zone = origin) |>
    select(zone, bk_p, da_p, rs_p, s2_p, s3_p, sb_p, ta_p, tw_p, wk_p)
  
  mc_by_a_zone <- mc_table |>
    group_by(destination) |>
    summarise(across(everything(), list(a = sum))) |>
    rename(zone = destination) |>
    select(zone, bk_a, da_a, rs_a, s2_a, s3_a, sb_a, ta_a, tw_a, wk_a)
  
  full_join(mc_by_p_zone, mc_by_a_zone) |>
    mutate(bk = bk_p + bk_a,
           da = da_p + da_a,
           rs = rs_p + rs_a,
           s2 = s2_p + s2_a,
           s3 = s3_p + s3_a,
           sb = sb_p + sb_a,
           ta = ta_p + ta_a,
           tw = tw_p + tw_a,
           wk = wk_p + wk_a) |>
    mutate(total = bk + da + rs + s2 + s3 + sb + ta + tw + wk) |>
    mutate(bk_pct = bk / total,
           da_pct = da / total,
           rs_pct = rs / total,
           s2_pct = s2 / total,
           s3_pct = s3 / total,
           sb_pct = sb / total,
           ta_pct = ta / total,
           tw_pct = tw / total,
           wk_pct = wk / total) |>
    select(zone,
           total,
           bk_pct,
           da_pct,
           rs_pct,
           s2_pct,
           s3_pct,
           sb_pct,
           ta_pct,
           tw_pct,
           wk_pct)
}

base_mc_by_zone <- mode_share_by_zone(base_mc)

alt_mc_by_zone <- mode_share_by_zone(alt_mc)

base_mc_by_zone <- base_mc_by_zone |>
  rename_with(~ paste0("base_", .), -all_of("zone"))

alt_mc_by_zone <- alt_mc_by_zone |>
  rename_with(~ paste0("alt_", .), -all_of("zone"))

mc_comparison <- full_join(base_mc_by_zone, alt_mc_by_zone) |>
  replace_na(list(base_total = 0,
                  alt_total = 0)) |>
  filter(alt_total + base_total > 0) |>
  mutate(diff_bk = alt_bk_pct - base_bk_pct,
         diff_da = alt_da_pct - base_da_pct,
         diff_rs = alt_rs_pct - base_rs_pct,
         diff_s2 = alt_s2_pct - base_s2_pct,
         diff_s3 = alt_s3_pct - base_s3_pct,
         diff_sb = alt_sb_pct - base_sb_pct,
         diff_ta = alt_ta_pct - base_ta_pct,
         diff_tw = alt_tw_pct - base_tw_pct,
         diff_wk = alt_wk_pct - base_wk_pct)

###### Visualize Drive Alone

drive_comparison <- here("C:/model/inputs/zonal/shp/CTPS_TDM23_TAZ_2017g_v202303.shp") |>
  st_read(quiet = TRUE) |>
  st_transform("WGS84") |>
  rename(zone = taz_id) |>
  left_join(mc_comparison) |>
  filter(abs(diff_da) > 0.01)

base_map <- get_tiles(drive_comparison,
                      provider = "CartoDB.Positron",
                      zoom = 13,
                      crop = TRUE)

ggplot(drive_comparison) +
  geom_spatraster_rgb(data = base_map) +
  geom_sf(aes(fill = diff_da),
          color = NA,
          alpha = 0.7) +
  scale_fill_gradient2(name = "Difference in share\nof drive-alone trips") +
  theme_void()


######### Visualize park and ride

parkride_comparison <- here("C:/model/inputs/zonal/shp/CTPS_TDM23_TAZ_2017g_v202303.shp") |>
  st_read(quiet = TRUE) |>
  st_transform("WGS84") |>
  rename(zone = taz_id) |>
  left_join(mc_comparison) |>
  filter(abs(diff_ta) > 0.01)

base_map <- get_tiles(parkride_comparison,
                      provider = "CartoDB.Positron",
                      zoom = 13,
                      crop = TRUE)

ggplot(parkride_comparison) +
  geom_spatraster_rgb(data = base_map) +
  geom_sf(aes(fill = diff_ta),
          color = NA,
          alpha = 0.7) +
  scale_fill_gradient2(name = "Difference in share\nof parkride trips") +
  theme_void()

####### Visualize Walk and Ride

walkride_comparison <- here("C:/model/inputs/zonal/shp/CTPS_TDM23_TAZ_2017g_v202303.shp") |>
  st_read(quiet = TRUE) |>
  st_transform("WGS84") |>
  rename(zone = taz_id) |>
  left_join(mc_comparison) |>
  filter(abs(diff_tw) > 0.01)

base_map <- get_tiles(walkride_comparison,
                      provider = "CartoDB.Positron",
                      zoom = 13,
                      crop = TRUE)

ggplot(walkride_comparison) +
  geom_spatraster_rgb(data = base_map) +
  geom_sf(aes(fill = diff_tw),
          color = NA,
          alpha = 0.7) +
  scale_fill_gradient2(name = "Difference in share\nof walkride trips") +
  theme_void()

###### Visualize Biking

bike_comparison <- here("C:/model/inputs/zonal/shp/CTPS_TDM23_TAZ_2017g_v202303.shp") |>
  st_read(quiet = TRUE) |>
  st_transform("WGS84") |>
  rename(zone = taz_id) |>
  left_join(mc_comparison) |>
  filter(abs(diff_bk) > 0.001)

base_map <- get_tiles(bike_comparison,
                      provider = "CartoDB.Positron",
                      zoom = 13,
                      crop = TRUE)

ggplot(bike_comparison) +
  geom_spatraster_rgb(data = base_map) +
  geom_sf(aes(fill = diff_bk),
          color = NA,
          alpha = 0.7) +
  scale_fill_gradient2(name = "Difference in share\nof bike trips") +
  theme_void()

##### Visualize Walking


walk_comparison <- here("C:/model/inputs/zonal/shp/CTPS_TDM23_TAZ_2017g_v202303.shp") |>
  st_read(quiet = TRUE) |>
  st_transform("WGS84") |>
  rename(zone = taz_id) |>
  left_join(mc_comparison) |>
  filter(abs(diff_wk) > 0.01)

base_map <- get_tiles(walk_comparison,
                      provider = "CartoDB.Positron",
                      zoom = 13,
                      crop = TRUE)

ggplot(walk_comparison) +
  geom_spatraster_rgb(data = base_map) +
  geom_sf(aes(fill = diff_wk),
          color = NA,
          alpha = 0.7) +
  scale_fill_gradient2(name = "Difference in share\nof walk trips") +
  theme_void()

