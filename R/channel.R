#' ch_station
#'
#' Reads channel.stod_v. Access the same view as mar::les_stod
#' 
#' @param con oracle connection
#' @param std create standardized/shortcut names (default is TRUE)
#' @param trim trim return only key variables (default is TRUE). only operational
#' if std is TRUE
#'
#' @return a tibble query
#' @export
#'
ch_station <- function(con, std = TRUE, trim = TRUE) {
  
  q <- tbl_mar(con, "channel.station_v")
  
  if(std) {
    q <- 
      q %>% 
      dplyr::select(stid = station_id,
                    vid = vessel_no,
                    date = station_date,
                    year,
                    lon = longitude,
                    lat = latitude,
                    lon2 = longitude_end,
                    lat2 = latitude_end,
                    z1 = bottom_depth,
                    z2 = bottom_depth_end,
                    st = surface_temp,
                    bt = bottom_temp,
                    dplyr::everything())
    if(trim) {
      q <- q %>% dplyr::select(stid:bt)
    }
  }
  
  return(q)
  
}

#' ch_sample
#'
#' Reads channel.syni_v
#' 
#' @param con oracle connection
#' @param std create standardized/shortcut names (default is TRUE)
#' @param trim trim return only key variables (default is TRUE). only operational
#' if std is TRUE
#'
#' @return a tibble query
#' @export
#'
ch_sample <- function(con, std = TRUE, trim = TRUE) {
  
  q <- tbl_mar(con, "channel.sample") %>% glimpse()
  
  if(std) {
    q <- 
      q %>% 
      dplyr::select(stid = stod_id,
                    id = sample_id,
                    gid = load_veidarfaeri,
                    sclass = sample_category_no,
                    t1 = tow_start,
                    t2 = tow_end,
                    speed,
                    towlength = distance,
                    dplyr::everything())
    if(trim) {
      q <- q %>% dplyr::select(stid:towlength)
    }
  }
  
  return(q)
  
}
