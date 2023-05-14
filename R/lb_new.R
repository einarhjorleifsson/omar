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