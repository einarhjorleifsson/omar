lb_trip_new <- function(con) {
  tbl_mar(con, "adb.trip_v") |> 
    dplyr::filter(departure >= to_date("2020-01-01", "YYYY:MM:DD")) |> 
    dplyr::select(tid = trip_id,
                  vid = vessel_no,
                  T1 = departure,
                  hid1 = departure_port_no,
                  T2 = landing,
                  hid2 = landing_port_no,
                  source)
}


lb_station_new <- function(con) {
  tbl_mar(con, "adb.station_v") |> 
    dplyr::select(tid = trip_id,
                  visir = station_id,
                  gid = gear_no,
                  t1 = fishing_start,
                  t2 = fishing_end,
                  lon = longitude,
                  lat = latitude,
                  lon2 = longitude_end,
                  lat2 = latitude_end,
                  z1 = depth,
                  z2 = depth_end)
  
}


#' The new logbook
#'
#' @param con oracle connection
#'
#' @return a query
#' @export
#'
lb_base_new <- function(con) {
  lb_trip_new(con) |> 
    dplyr::left_join(lb_station_new(con))
}

#' Logbook catch by species
#'
#' @param con oracle connection
#'
#' @return sql query and data
#' @export
#'
lb_catch_new <- function(con) {
  q <-
    tbl_mar(con, "adb.catch_v")
    dplyr::select(station_id,
                  sid = species_no,
                  catch = weight)
  return(q)
}

#' Logbook mobile (active) gear
#'
#' @param con oracle connection
#' @param correct_gear a boolean (default TRUE) checks for lookup-table for
#' gear correction (adds variable "gidc" to the tibble)
#' @param trim trim variables returned (default TRUE)
#'
#' @return a sql tibble
#' @export

lb_mobile_new <- function(con, standaridize = TRUE) {
  
  q <- 
    tbl_mar(con, "adb.trawl_and_seine_net_v")
  q |> glimpse()
  
  q <- 
    lb_base_() %>% 
    dplyr::inner_join(tbl_mar(con, "afli.toga"),
                      by = "visir")
  
  q <-
    q %>% 
    dplyr::rename(towtime = togtimi,
                  on.bottom = ibotni) %>%
    dplyr::mutate(effort = dplyr::case_when(gid2 %in% c(6, 7, 15) ~ towtime / 60,
                                            # for seine and traps use setting as effort
                                            gid2 %in% c(5, 17) ~ 1,
                                            TRUE ~ NA_real_),
                  effort_unit = dplyr::case_when(gid2 %in% c(6, 7, 9, 14, 15, 38, 40) ~ "hours towed",
                                                 # for seine just use the setting
                                                 gid2 %in% c(5, 17) ~ "setting",
                                                 TRUE ~ NA_character_)) %>%
    dplyr::mutate(on.bottom = lpad(on.bottom, 4, "0")) %>%
    # vedags + (substr(lpad(ibotni,4,'0'),1,2)*60+substr(lpad(ibotni,4,'0'),3,2))/24/60 t1
    # Oracle time is in days
    dplyr::mutate(t1 = date + (substr(on.bottom, 1, 2) * 60 + substr(on.bottom, 3, 4)) / (24 * 60),
                  t2 = date + (substr(on.bottom, 1, 2) * 60 + substr(on.bottom, 3, 4) + towtime) / (24 * 60)) %>% 
    dplyr::select(visir, vid, gid, year:hid, 
                  towtime,                     # in minutes
                  effort,
                  effort_unit,
                  mesh = moskvi,
                  mesh_min = moskvi_minnsti,
                  doors = hlerar,              # in kilograms
                  headline = hoflina,
                  sweeps = grandarar,          # in meters ???
                  plow_width = pl_breidd,
                  tempb1 = botnhiti,           # bottom temperature
                  tempb2 = botnhiti_lok,
                  temps1 = uppsj_hiti,         # surface temperature
                  temps2 = uppsj_hiti_lok,
                  t1, 
                  t2,
                  on.bottom,
                  dplyr::everything())
  
  if(trim) {
    q <-
      q %>% 
      dplyr::select(visir:on.bottom)
  }
  
  return(q)
}

