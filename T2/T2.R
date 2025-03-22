##########################################
# Load libraries

library(tidyverse)
library(sf)
library(DBI)
library(here)
library(leaflet)
library(maptiles)
library(tidyterra)
library(knitr)
library(tigris)

sf::sf_use_s2(FALSE)

##########################################
# Look at base scenario for home-based trip productions

base_db <- dbConnect(drv=RSQLite::SQLite(), 
                     dbname=here("C:/model/outputs/Base/tdm23.db"))

base_trip_prod <- dbGetQuery(base_db, 'SELECT * FROM trip_prod')

dbDisconnect(base_db)

base_block_prod <- base_trip_prod |>
  replace_na(list(hbw_p = 0,
                  hbsc_p = 0,
                  hbsr_p = 0,
                  hbpb_p = 0,
                  nhbw_p = 0,
                  nhbnw_p = 0)) |>
  group_by(block_id) |>
  summarise(hbw_p = sum(hbw_p),
            hbsc_p = sum(hbsc_p),
            hbsr_p = sum(hbsr_p),
            hbpb_p = sum(hbpb_p),
            nhbw_p = sum(nhbw_p),
            nhbnw_p = sum(nhbnw_p))

##########################################
# Look at alternative/recession scenario for home-based trip productions

alt_db <- dbConnect(drv=RSQLite::SQLite(), 
                    dbname=here("C:/model/outputs/recession/tdm23.db"))

alt_trip_prod <- dbGetQuery(alt_db, 'SELECT * FROM trip_prod')

dbDisconnect(alt_db)

alt_block_prod <- alt_trip_prod |>
  replace_na(list(hbw_p = 0,
                  hbsc_p = 0,
                  hbsr_p = 0,
                  hbpb_p = 0,
                  nhbw_p = 0,
                  nhbnw_p = 0)) |>
  group_by(block_id) |>
  summarise(hbw_p_alt = sum(hbw_p),
            hbsc_p_alt = sum(hbsc_p),
            hbsr_p_alt = sum(hbsr_p),
            hbpb_p_alt = sum(hbpb_p),
            nhbw_p_alt = sum(nhbw_p),
            nhbnw_p_alt = sum(nhbnw_p))

##########################################
# Set tolerance level, filter data by tolerance

tolerance_small <- 0.05
tolerance_med <- 0.5
tolerance_bigboy <- 1

compare_prod <- full_join(base_block_prod, alt_block_prod) |>
  mutate(dif_hbw_p = (hbw_p_alt - hbw_p)/hbw_p,
         dif_hbsc_p = (hbsc_p_alt - hbsc_p)/hbsc_p,
         dif_hbsr_p = (hbsr_p_alt - hbsr_p)/hbsr_p,
         dif_hbpb_p = (hbpb_p_alt - hbpb_p)/hbpb_p)

compare_prod_filtered_small <- compare_prod |>
  filter(abs(dif_hbw_p) > tolerance_small |
           abs(dif_hbsc_p) > tolerance_small |
           abs(dif_hbsr_p) > tolerance_small |
           abs(dif_hbpb_p) > tolerance_small)

compare_prod_filtered_med <- compare_prod |>
  filter(abs(dif_hbw_p) > tolerance_med |
           abs(dif_hbsc_p) > tolerance_med |
           abs(dif_hbsr_p) > tolerance_med |
           abs(dif_hbpb_p) > tolerance_med)

compare_prod_filtered_bigboy <- compare_prod |>
  filter(abs(dif_hbw_p) > tolerance_bigboy |
           abs(dif_hbsc_p) > tolerance_bigboy |
           abs(dif_hbsr_p) > tolerance_bigboy |
           abs(dif_hbpb_p) > tolerance_bigboy)

##########################################
# Create map showing differences in home-based trip productions

ma_blocks <- blocks(state = "MA", 
                    year = 2010,
                    progress_bar = FALSE) |>
  st_transform("WGS84") |>
  rename(block_id = GEOID10) |>
  select(block_id)

TAZs <- here("model",
             "inputs",
             "zonal",
             "shp",
             "CTPS_TDM23_TAZ_2017g_v202303.shp") |>
  st_read(quiet = TRUE) |>
  st_transform("WGS84") |>
  mutate(block_id = as.character(taz_id)) |>
  select(block_id)

zones <- rbind(ma_blocks, TAZs)

dif_blocks_prod <- zones |>
  right_join(compare_prod) 

hb_prod_labels <- paste0(round((dif_blocks_prod$dif_hbw_p)*100),
                         "% change in HBW trips<br/>",
                         round((dif_blocks_prod$dif_hbsc_p)*100), 
                         "% change in HB-school trips<br/>",
                         round((dif_blocks_prod$dif_hbsr_p)*100),
                         "% change in HB-soc/rec trips<br/>",
                         round(dif_blocks_prod$dif_hbpb_p*100),
                         "% change in HB-personal-business") |>
  lapply(htmltools::HTML)

leaflet(dif_blocks_prod) |>
  addProviderTiles(provider = "CartoDB.Positron") |>
  addPolygons(weight = 2,
              color = "forestgreen",
              fillColor = "forestgreen",
              fillOpacity = 0.1,
              highlightOptions = highlightOptions(weight = 3,
                                                  fillOpacity = 0.5),
              label = hb_prod_labels)

##########################################
# Look at base scenario for trip attractions

base_db <- dbConnect(drv=RSQLite::SQLite(), 
                     dbname=here("C:/model/outputs/Base/tdm23.db"))

base_trip_attr <- dbGetQuery(base_db, 'SELECT * FROM trip_attr')

dbDisconnect(base_db)

##########################################
# Look at alternative/recession scenario for trip attractions

alt_db <- dbConnect(drv=RSQLite::SQLite(), 
                    dbname=here("C:/model/outputs/recession/tdm23.db"))

alt_trip_attr <- dbGetQuery(alt_db, 'SELECT * FROM trip_attr') |>
  rename_with(~ paste0("alt_", .x),
              ends_with("_a")) 

dbDisconnect(alt_db)

##########################################
# Filter data by tolerance

compare_attr <- full_join(base_trip_attr, alt_trip_attr) |>
  mutate(diff_hbw_inc1_a = (alt_hbw_inc1_a - hbw_inc1_a)/hbw_inc1_a,
         diff_hbw_inc2_a = (alt_hbw_inc2_a - hbw_inc2_a)/hbw_inc2_a,
         diff_hbw_inc3_a = (alt_hbw_inc3_a - hbw_inc3_a)/hbw_inc3_a,
         diff_hbw_inc4_a = (alt_hbw_inc4_a - hbw_inc4_a)/hbw_inc4_a,
         diff_hbsr_a = (alt_hbsr_a - hbsr_a)/hbsr_a,
         diff_hbsc_a = (alt_hbsc_a - hbsc_a)/hbsc_a,
         diff_hbpb_a = (alt_hbpb_a - hbpb_a)/hbpb_a,
         diff_nhbw_a = (alt_nhbw_a - nhbw_a)/nhbw_a,
         diff_nhbnw_a = (alt_nhbnw_a - nhbnw_a)/nhbnw_a)

compare_attr_filtered_small <- compare_attr |>
  filter(abs(diff_hbw_inc1_a) >= tolerance_small |
           abs(diff_hbw_inc2_a) >= tolerance_small |
           abs(diff_hbw_inc3_a) >= tolerance_small |
           abs(diff_hbw_inc4_a) >= tolerance_small |
           abs(diff_hbsc_a) >= tolerance_small |
           abs(diff_hbsr_a) >= tolerance_small |
           abs(diff_hbpb_a) >= tolerance_small |
           abs(diff_nhbw_a) >= tolerance_small |
           abs(diff_nhbnw_a) >= tolerance_small)

compare_attr_filtered_med <- compare_attr |>
  filter(abs(diff_hbw_inc1_a) >= tolerance_med |
           abs(diff_hbw_inc2_a) >= tolerance_med |
           abs(diff_hbw_inc3_a) >= tolerance_med |
           abs(diff_hbw_inc4_a) >= tolerance_med |
           abs(diff_hbsc_a) >= tolerance_med |
           abs(diff_hbsr_a) >= tolerance_med |
           abs(diff_hbpb_a) >= tolerance_med |
           abs(diff_nhbw_a) >= tolerance_med |
           abs(diff_nhbnw_a) >= tolerance_med)

compare_attr_filtered_bigboy <- compare_attr |>
  filter(abs(diff_hbw_inc1_a) >= tolerance_bigboy |
           abs(diff_hbw_inc2_a) >= tolerance_bigboy |
           abs(diff_hbw_inc3_a) >= tolerance_bigboy |
           abs(diff_hbw_inc4_a) >= tolerance_bigboy |
           abs(diff_hbsc_a) >= tolerance_bigboy |
           abs(diff_hbsr_a) >= tolerance_bigboy |
           abs(diff_hbpb_a) >= tolerance_bigboy |
           abs(diff_nhbw_a) >= tolerance_bigboy |
           abs(diff_nhbnw_a) >= tolerance_bigboy)

##########################################
# Create map showing differences in trip attractions

attr_labels <- paste0(formatC(dif_blocks_attr$diff_hbw_inc1_a*100, format = "f", digits = 2),
                      "% change in HBW trips (< $35k)<br/>",
                      formatC(dif_blocks_attr$diff_hbw_inc2_a*100, format = "f", digits = 2),
                      "% change in HBW trips ($35k - $65k)<br/>",
                      formatC(dif_blocks_attr$diff_hbw_inc3_a*100, format = "f", digits = 2),
                      "% change in HBW trips ($65k - $100k)<br/>",
                      formatC(dif_blocks_attr$diff_hbw_inc4_a*100, format = "f", digits = 2),
                      "% change in HBW trips (> $100k)<br/>",
                      formatC(dif_blocks_attr$diff_hbsc_a*100, format = "f", digits = 2), 
                      "% change in HB-school trips<br/>",
                      formatC(dif_blocks_attr$diff_hbsr_a*100, format = "f", digits = 2),
                      "% change in HB-soc/rec trips<br/>",
                      formatC(dif_blocks_attr$diff_hbpb_a*100, format = "f", digits = 2),
                      "% change in HB-per-bus<br/>",
                      formatC(dif_blocks_attr$diff_nhbw_a*100, format = "f", digits = 2),
                      "% non-home-based work trips<br/>",
                      formatC(dif_blocks_attr$diff_nhbnw_a*100, format = "f", digits = 2),
                      "% change in non-home-based non-work trips") |>
  lapply(htmltools::HTML)

leaflet(dif_blocks_attr) |>
  addProviderTiles(provider = "CartoDB.Positron") |>
  addPolygons(weight = 2,
              color = "forestgreen",
              fillColor = "forestgreen",
              fillOpacity = 0.1,
              highlightOptions = highlightOptions(weight = 3,
                                                  fillOpacity = 0.5),
              label = attr_labels)

##########################################
#  Look at base scenario for non-home-based trip productions

base_db <- dbConnect(drv=RSQLite::SQLite(), 
                     dbname=here("C:/model/outputs/Base/tdm23.db"))

base_trip_nhb <- dbGetQuery(base_db, 'SELECT * FROM prod_nhb')

dbDisconnect(base_db)

##########################################
# Look at alternative/recession scenario for non-home-based trip productions

alt_db <- dbConnect(drv=RSQLite::SQLite(), 
                    dbname=here("C:/model/outputs/recession/tdm23.db"))

alt_trip_nhb <- dbGetQuery(alt_db, 'SELECT * FROM prod_nhb') |>
  rename_with(~ paste0("alt_", .x),
              ends_with("_p")) 

dbDisconnect(alt_db)

##########################################
# Filter data by tolerance

compare_nhb <- full_join(base_trip_nhb, alt_trip_nhb) |>
  mutate(diff_nhbw_p = (alt_nhbw_p - nhbw_p)/nhbw_p,
         diff_nhbnw_p = (alt_nhbnw_p - nhbnw_p)/nhbnw_p)

compare_nhb_filtered_small <- compare_nhb |>
  filter(abs(diff_nhbw_p) >= tolerance_small |
           abs(diff_nhbnw_p) >= tolerance_small)

compare_nhb_filtered_med <- compare_nhb |>
  filter(abs(diff_nhbw_p) >= tolerance_med |
           abs(diff_nhbnw_p) >= tolerance_med)

compare_nhb_filtered_bigboy <- compare_nhb |>
  filter(abs(diff_nhbw_p) >= tolerance_bigboy |
           abs(diff_nhbnw_p) >= tolerance_bigboy)

##########################################
# Create map showing differences in non-home-based trip productions

dif_blocks_nhb <- zones |>
  right_join(compare_nhb) 

nhb_labels <- paste0(formatC(dif_blocks_nhb$diff_nhbw_p*100, format = "f", digits = 2),
                     " non-home-based work trips<br/>",
                     formatC(dif_blocks_nhb$diff_nhbnw_p*100, format = "f", digits = 2),
                     "% change in non-home-based non-work trips") |>
  lapply(htmltools::HTML)

leaflet(dif_blocks_nhb) |>
  addProviderTiles(provider = "CartoDB.Positron") |>
  addPolygons(weight = 2,
              color = "forestgreen",
              fillColor = "forestgreen",
              fillOpacity = 0.1,
              highlightOptions = highlightOptions(weight = 3,
                                                  fillOpacity = 0.5),
              label = nhb_labels)