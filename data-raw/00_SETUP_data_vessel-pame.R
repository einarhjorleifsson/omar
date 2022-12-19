library(lubridate)
library(tidyverse)
library(countrycode)
library(omar)
source("R/vid.R")
con <- connect_mar()
VESSEL.PAME <- 
  tbl_mar(con, "pame.astd_area_level1") %>% 
  select(mmsi,
         imo = imonumber,
         vessel = vesselname,
         flag = flagname,
         astd = astd_cat,
         time = date_time_utc) %>% 
  group_by(mmsi, imo, vessel, flag, astd) %>% 
  summarise(n.pings = n(),
            tmin = min(time),
            tmax = max(time),
            .groups = "drop") %>% 
  collect(n = Inf) %>% 
  mutate(imo2 = ifelse(between(imo, 1000002, 9999998), imo, NA),
         imo2 = ifelse(imo2 %in% c(1234567), NA, imo2),
         imo.valid = valid_imo(imo2),
         mmsi2 = ifelse(between(mmsi, 100000002,999999998), mmsi, NA),
         mmsi2 = ifelse(mmsi2 == 111111111, NA, mmsi),
         astd = ifelse(astd == "Unknown", NA, astd))
VESSEL.PAME <- 
  VESSEL.PAME %>%
  arrange(mmsi2, tmin, imo2, vessel, flag, astd) %>% 
  group_by(mmsi2) %>% 
  fill(vessel, .direction = "downup") %>% 
  fill(flag, .direction = "downup") %>% 
  fill(astd, .direction = "downup") %>% 
  fill(imo2, .direction = "downup") %>% 
  ungroup() %>% 
  group_by(mmsi2, imo2, vessel, flag, astd) %>% 
  summarise(n.pings = sum(n.pings),
            tmin = min(tmin),
            tmax = max(tmax),
            .groups = "drop")
# one record per mmsi and imo, last value for vessel and astd
VESSEL.PAME <- 
  VESSEL.PAME %>% 
  group_by(mmsi2, imo2) %>% 
  summarise(#vessel.p = vessel[n.pings = max(n.pings)],
            #flag.p = flag[n.pings = max(n.pings)],
            #astd.p = astd[n.pings = max(n.pings)],
            n.pings = sum(n.pings),
            vessel = vessel[tmax == max(tmax)],
            flag = flag[tmax == max(tmax)],
            astd = astd[tmax == max(tmax)],
            tmin = min(tmin),
            tmax = max(tmax),
            .groups = "drop") %>% 
  ungroup()
usethis::use_data(VESSEL.PAME, overwrite = TRUE)
