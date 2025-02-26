#######################################
# Load libraries and functions

library(tidyverse)
library(here)
library(sf)
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

'%!in%' <- function(x,y)!('%in%'(x,y))

#######################################
# Load TAZs and files

TAZs <- here("model",
             "inputs",
             "zonal",
             "shp",
             "CTPS_TDM23_TAZ_2017g_v202303.shp") |>
  st_read(quiet = TRUE) |>
  st_transform("WGS84")

MA_pop <- here("model",
               "inputs",
               "zonal",
               "ma_population_run97-176_2019_v20240109.csv") |>
  read_csv(show_col_types = FALSE) |>
  mutate(block_id = as.character(block_id))

RINH_pop <- here("model",
                 "inputs",
                 "zonal",
                 "nhri_population_2020_v20230518.csv") |>
  read_csv(show_col_types = FALSE)

#######################################
# Read in MA employment file

MA_emp <- here("model",
               "inputs",
               "zonal",
               "ma_employment_run97-176_2019_v20240109.csv") |>
  read_csv(show_col_types = FALSE) |>
  mutate(`1_constr` = as.numeric(`1_constr`),
         `2_eduhlth` = as.numeric(`2_eduhlth`),
         `3_finance` = as.numeric(`3_finance`),
         `4_public` = as.numeric(`4_public`),
         `5_info` = as.numeric(`5_info`),
         `6_ret_leis` = as.numeric(`6_ret_leis`),
         `7_manu` = as.numeric(`7_manu`),
         `8_other` = as.numeric(`8_other`),
         `9_profbus` = as.numeric(`9_profbus`),
         `10_ttu` = as.numeric(`10_ttu`),
         block_id = as.character(`block_id`))

#######################################
# Read in RINH employment file

RINH_emp <- here("model",
                 "inputs",
                 "zonal",
                 "nhri_employment_2020_v20230518.csv") |>
  read_csv(show_col_types = FALSE) |>
  mutate(`1_constr` = as.numeric(`1_constr`),
         `2_eduhlth` = as.numeric(`2_eduhlth`),
         `3_finance` = as.numeric(`3_finance`),
         `4_public` = as.numeric(`4_public`),
         `5_info` = as.numeric(`5_info`),
         `6_ret_leis` = as.numeric(`6_ret_leis`),
         `7_manu` = as.numeric(`7_manu`),
         `8_other` = as.numeric(`8_other`),
         `9_profbus` = as.numeric(`9_profbus`),
         `10_ttu` = as.numeric(`10_ttu`),
         block_id = as.character(`block_id`))

#######################################
# Cut education and health jobs in MA by 90%

MA_emp <- MA_emp |> 
  mutate(`2_eduhlth` = `2_eduhlth` %/% 10) |>
  mutate(total_jobs = `1_constr` +
           `2_eduhlth` +
           `3_finance` +
           `4_public` +
           `5_info` +
           `6_ret_leis` +
           `7_manu` +
           `8_other` +
           `9_profbus` +
           `10_ttu`)

#######################################
# Cut education and health jobs in RINH by 90%

RINH_emp <- RINH_emp |> 
  mutate(`2_eduhlth` = `2_eduhlth` %/% 10) |>
  mutate(total_jobs = `1_constr` +
           `2_eduhlth` +
           `3_finance` +
           `4_public` +
           `5_info` +
           `6_ret_leis` +
           `7_manu` +
           `8_other` +
           `9_profbus` +
           `10_ttu`)

#######################################
# Write alternative .csv files

write.csv(MA_emp, "C:\\model\\inputs\\zonal\\recession_MA_emp.csv", row.names=FALSE)
write.csv(RINH_emp, "C:\\model\\inputs\\zonal\\recession_RINH_emp.csv", row.names=FALSE)

#######################################
# Load base model output

base_db <- dbConnect(drv=RSQLite::SQLite(), 
                     dbname=here("model",
                                 "outputs",
                                 "Base",
                                 "tdm23.db"))

my_query <- paste0("SELECT * FROM veh")

MARINH_vehs_base <- dbGetQuery(base_db, my_query)

dbDisconnect(base_db)

#######################################
# Load recession model output

alt_db <- dbConnect(drv=RSQLite::SQLite(), 
                    dbname=here("model",
                                "outputs",
                                "recession",
                                "tdm23.db"))

MARINH_vehs_recession <- dbGetQuery(alt_db, my_query)

dbDisconnect(alt_db)

#######################################
# Join MA and RINH data

MARINH_vehs_base <- MARINH_vehs_base |>
  rename(veh_suff_base = veh_suff) |>
  select(-num_vehs)

MARINH_vehs_recession <- MARINH_vehs_recession |>
  rename(veh_suff_recession = veh_suff) |>
  select(-num_vehs)

MARINH_vehs_dif <- full_join(MARINH_vehs_base, MARINH_vehs_recession) |>
  mutate(veh_suff_base = ifelse(is.na(veh_suff_base), "new", veh_suff_base))

#######################################
# Categorize changes to vehicle availability

MARINH_vehs_changed_hh <- MARINH_vehs_dif |>
  mutate(change = case_when(veh_suff_base == "zv" & veh_suff_recession == "iv" ~
                              "gained vehicle",
                            veh_suff_base == "zv" & veh_suff_recession == "sv" ~
                              "gained vehicle",
                            veh_suff_base == "sv" & veh_suff_recession == "zv" ~
                              "lost vehicle",
                            veh_suff_base == "sv" & veh_suff_recession == "iv" ~
                              "lost vehicle",
                            veh_suff_base == "iv" & veh_suff_recession == "zv" ~
                              "lost vehicle",
                            veh_suff_base == "iv" & veh_suff_recession == "sv" ~
                              "gained vehicle",
                            TRUE ~ "no change"))

MARINH_vehs_changed_blocks <- MARINH_vehs_changed_hh |>
  group_by(block_id) |>
  summarise(n_hhs = n(),
            n_gained_veh = sum(change == "gained vehicle"),
            n_lost_veh = sum(change == "lost vehicle")) |>
  filter(n_lost_veh +
           n_gained_veh > 0) |>
  rename(id = block_id)

#######################################
# Get geometries

blocks <- blocks(state = "MA", progress_bar = FALSE, year = 2010) |>
  rename(id = GEOID10) |>
  select(id) |>
  st_transform("WGS84")

TAZs <- here("model",
             "inputs",
             "zonal",
             "shp",
             "CTPS_TDM23_TAZ_2017g_v202303.shp") |>
  st_read(quiet = TRUE) |>
  st_transform("WGS84") |>
  rename(id = taz_id) |>
  select(id)

MA_pts <- st_centroid(TAZs) |>
  st_filter(blocks)

RI_NH_TAZs <- TAZs |>
  filter(id %!in% MA_pts$id) #THIS WORKS

all_zones <- rbind(blocks, RI_NH_TAZs) #THIS WORKS

dif_blocks <- all_zones |>
  right_join(MARINH_vehs_changed_blocks)

veh_labels <- paste0(dif_blocks$n_hhs,
                     " total households<br/>",
                     dif_blocks$n_lost_veh,
                     " households lost a vehicle<br/>",
                     dif_blocks$n_gained_veh,
                     " households gained a vehicle") |>
  lapply(htmltools::HTML)

leaflet(dif_blocks) |>
  addProviderTiles(provider = "CartoDB.Positron") |>
  addPolygons(weight = 2,
              color = "forestgreen",
              fillColor = "forestgreen",
              fillOpacity = 0.1,
              highlightOptions = highlightOptions(weight = 3,
                                                  fillOpacity = 0.5),
              label = veh_labels)

#######################################
# Write to shape files

st_write(dif_blocks,
         here("T1",
              "dif_blocks"),
         driver = "ESRI Shapefile")
