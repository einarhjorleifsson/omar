#' Pame vessel id
#' 
#' A table of Icelandic vessels id (vid) that are in pame, corresponding mmsi and
#' the number of records
#' 
#' @param con Oracle connection
#' 
#' @return a query
#' @export

pame_vid_mmsi <- function(con) {
  tbl_mar(con, "ops$einarhj.pame_vid_mmsi")
}

#' Connect to pame trail
#'
#' @param con Oracle connection
#' @param std Boolean (default TRUE)
#' @param trim Boolean (default TRUE)
#'
#' @return A query
#' @export
pame_trail <- function(con, std = TRUE, trim = TRUE) {
  
  q <- tbl_mar(con, "pame.astd_v")
  
  if(std) {
    q <- 
      q |> 
      dplyr::select(mmsi,
                  imo = imonumber,
                  time = date_time_utc,
                  vessel = vesselname,
                  flag = flagname,
                  lon = longitude,
                  lat = latitude,
                  dplyr::everything())
  }
  
  if(std & trim) {
    q <-
      q |> 
      dplyr::select(mmsi:lat) |> 
      dplyr::mutate(lon = ifelse(lon > 10 & lat < 67, -lon, lon))
  }
  return(q)
}
