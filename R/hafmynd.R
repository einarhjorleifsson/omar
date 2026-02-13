hm_cruise <- function(con, trim = TRUE) {
  tbl_mar(con, "hafmynd.cruise") |> 
    dplyr::select(.cid = id,
           cruise = name,
           project_id)
}
hm_station <- function(con, trim = TRUE) {
  tbl_mar(con, "hafmynd.station") |> 
    dplyr::select(.cid = cruise_id,
                  .stid = station_number,
                  time = station_timestamp,
                  lon = longitude,
                  lat = latitude,
                  .trid = track_source_id)
}
hm_tracks <- function(con, trim = TRUE) {
  tbl_mar(con, "hafmynd.station_track") |> 
    dplyr::select(.stid = station_id,
                  track_time = track_timestamp,
                  lon = longitude,
                  lat = latitude,
                  z = depth_m)
}
hm_image <- function(con, trim = TRUE) {
  tbl_mar(con, "hafmynd.image") |> 
    dplyr::select(.stid = station_id,
                  path = file_path,
                  i_time = image_timestamp,
                  area_m2,
                  scale_marker_cm,
                  scale_marker_pixels,
                  analysis_done,
                  source_video_id)
}
