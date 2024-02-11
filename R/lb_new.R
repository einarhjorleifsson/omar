adb_trip <- function(con) {
  tbl_mar(con, "adb.trip_v") |> 
    dplyr::filter(departure >= to_date("2020-01-01", "YYYY:MM:DD")) |> 
    dplyr::select(trip_id,
                  vid = vessel_no,
                  d1 = departure,
                  hid1 = departure_port_no,
                  d2 = landing,
                  hid2 = landing_port_no,
                  source)
}


adb_station <- function(con) {
  tbl_mar(con, "adb.station_v") |> 
    dplyr::select(trip_id,
                  station_id,
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
  adb_trip(con) |> 
    dplyr::left_join(adb_station(con),
                     by = "trip_id")
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
    tbl_mar(con, "adb.catch_v") |> 
    dplyr::mutate(catch = dplyr::case_when(condition == "GUTT" ~ quantity / 0.8,
                                           condition == "UNGU" ~ quantity,
                                           .default = NA)) |> 
    dplyr::select(station_id,
                  sid = species_no,
                  catch)
  
  return(q)
}

#' Logbook new - trawl and seine
#'
#' @param con oracle connection
#' @param trim trim variables returned (default TRUE)
#'
#' @return a sql tibble
#' @export

lb_mobile_new <- function(con, trim = TRUE) {
  
  # name of function a misnomer
  
  q <- 
    lb_base_new(con) %>% 
    # the only gears that are supposed to be in mobile (9 and 14 not there yet)
    dplyr::filter(gid %in% c(5, 6, 7, 9, 14)) |> 
    dplyr::left_join(tbl_mar(con, "adb.trawl_and_seine_net_v") |> 
                       dplyr::mutate(child = "yes"),
                     by = "station_id") |> 
    dplyr::mutate(effort = dplyr::case_when(gid == 5 ~ 1,
                                            .default = (t2 - t1) * 24),
                  child = ifelse(is.na(child), "no", child)) |> 
    dplyr::select(station_id, 
                  vid,
                  gid:z2,
                  effort,
                  mesh = mesh_size,
                  doors = otterboard_weight,              # in kilograms
                  headline = headline_length,
                  sweeps = bridle_length,          # in meters ??
                  dplyr::everything())
  
  if(trim) {
    q <-
      q %>% 
      # should possibly only return station_id:effort as standard
      dplyr::select(station_id:sweeps, source)
  }
  
  return(q)
  
}

#' Logbook new - trawl and seine
#'
#' @param con oracle connection
#' @param trim trim variables returned (default TRUE)
#'
#' @return a sql tibble
#' @export

lb_static_new <- function(con, trim = TRUE) {
  
  q <- 
    lb_base_new(con) %>% 
    dplyr::filter(gid %in% c(1, 2, 3, 
                             11,        # reknet 
                             25,        # grasleppunet
                             29,        # rauðmaganet
                             91,        # skötuselsnet
                             92)) |>    # graludunet
    dplyr::left_join(tbl_mar(con, "adb.line_and_net_v"),
                     by = "station_id") |> 
    dplyr::mutate(effort = 
                    dplyr::case_when(gid %in% c(1) ~ hooks,                            
                                     gid %in% c(2, 11, 25, 29, 91, 92) ~ nets,     # should really be netnights
                                     gid %in% c(3) ~ 4,  # 4 "faeri" - should really be hookhours
                                     .default = NA)) |> 
    dplyr::select(station_id, 
                  vid,
                  gid:z2,
                  effort,
                  mesh = mesh_size,
                  dplyr::everything())
  
  if(trim) {
    q <-
      q %>% 
      dplyr::select(station_id:effort, source = source)
  }
  
  return(q)
}

lb_dredge_new <- function(con) {
  q <- 
    lb_base_new(con) %>% 
    dplyr::inner_join(tbl_mar(con, "adb.dredge_v"),
                      by = "station_id")
}