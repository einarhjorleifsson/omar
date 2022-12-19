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
  
  q <- tbl_mar(con, "channel.stod_v")
  
  if(std) {
    q <- 
      q %>% 
      dplyr::select(.stid = stod_id,
                    vid = skip_nr,
                    date = dags,
                    year = ar,
                    sq = reitur,
                    ssq = smareitur,
                    lon = kastad_lengd,
                    lat = kastad_breidd,
                    lon2 = hift_lengd,
                    lat2 = hift_breidd,
                    z1 = botndypi_kastad,
                    z2 = botndypi_hift,
                    st = yfirbordshiti,
                    bt = botnhiti,
                    dplyr::everything())
    if(trim) {
      q <- q %>% dplyr::select(.stid:bt)
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
#' @return a tibble query, note towlength is in meters is std is set to TRUE
#' @export
#'
ch_sample <- function(con, std = TRUE, trim = TRUE) {
  
  q <- tbl_mar(con, "channel.syni_v")
  
  if(std) {
    q <- 
      q %>% 
      dplyr::select(.stid = stod_id,
                    .id = synis_id,
                    gid = veidarfaeri,
                    sclass = synaflokkur_nr,
                    tow_id = tog_nr,
                    t1 = togbyrjun,
                    t2 = togendir,
                    tid = tog_nr,
                    speed = toghradi,
                    towlength = toglengd,
                    dplyr::everything()) %>% 
      # go metric
      dplyr::mutate(towlength = 1852 * towlength)
    if(trim) {
      q <- q %>% dplyr::select(.stid:towlength)
    }
  }
  
  return(q)
  
}


