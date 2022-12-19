library(tidyverse)
library(omar)
con <- connect_mar()
vessel <-
  omar::vid_registry(con) |>
  select(vid, vessel) |>
  filter(!is.na(vid)) |>
  left_join(omar::mmsi_vessel(con) |>
              select(vid, mmsi)) |>
  collect(n = Inf) |>
  filter(!is.na(mmsi))
pame.mmsi <-
  omar::pam_trail(con) |>
  filter(between(lon, -29, -10),
         between(lat,  62.5, 68)) |>
  count(mmsi) |>
  collect(n = Inf)
vid.mmsi.pame <-
  vessel |>
  mutate(mmsi = as.integer(mmsi)) |>
  left_join(pame.mmsi |>
              rename(n.mmsi = n)) |>
  filter(!is.na(n.mmsi)) |>
  arrange(vid) |> 
  select(VID = vid, MMSI = mmsi, N_MMSI = n.mmsi)
DBI::dbWriteTable(con, name = "PAME_VID_MMSI", value = vid.mmsi.pame, overwrite = TRUE)
