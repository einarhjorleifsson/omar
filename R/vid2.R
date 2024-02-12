# The new vessel schema
#  2024-02-12 Try to emulate what is now in vessel_vessels


#' The new vessel stuff
#'
#' @param con oracle connection
#'
#' @return a query
#' @export
vessel <- function(con) {

  q <-
    tbl_mar(con, "vessel.vessel") |>
    dplyr::select(vid = registration_no,
                  vessel = name,
                  mclass = usage_category_no,   # mclass
                  imo = imo_no,
                  power_kw,
                  .vid = vessel_id) |> 
    dplyr::left_join(mmsi_icelandic_registry(con) |> 
                       dplyr::filter(!is.na(vid)) |> 
                       dplyr::select(vid, mmsi),
                     by = dplyr::join_by(vid)) |> 
    dplyr::left_join(v_identification(con),
                     by = dplyr::join_by(.vid)) |> 
    dplyr::left_join(v_measurements(con),
                     by = dplyr::join_by(.vid)) |> 
    dplyr::select(vid, vessel, uid, uno,
                  length,
                  mclass,
                  # date_mclass
                  cs,
                  imo,
                  mmsi,
                  length_reg,
                  gv,
                  gt,
                  width_reg,
                  depth_reg,
                  port)
                  #propeller_diameter)

  return(q)

}


#' vessel identifications
#'
#' @param con oracle connection
#'
#' @return a tibble
v_identification <- function(con) {
  q <-
    tbl_mar(con, "vessel.vessel_identification") |> 
    dplyr::select(uid = region_acronym,
                  uno = region_no,
                  cs = call_sign,
                  port = home_port,
                  .vid = vessel_id)
  
  return(q)
}

#' Title
#'
#' @param con oracle connection
#'
#' @return a tibble
v_measurements <- function(con) {
  
  q <- 
    tbl_mar(con, "vessel.vessel_measurement") |> 
    dplyr::select(
      length,
      gv =  brutto_grt_older,    # ???
      gt =  brutto_grt,          # ???
      length_reg = max_length,
      width_reg = width,
      depth_reg = depth,
      .vid = vessel_id)
  
  return(q)
  
}
    
# tbl_mar(con, "vessel.vessel_operational") |> collect() |> count(operational_category)
# tbl_mar(con, "vessel.vessel_fishery") |> glimpse()
# tbl_mar(con, "vessel.vessel_engines") |> glimpse()
# tbl_mar(con, "vessel.vessel_owners") |> glimpse()

# 
# 
# vessel_hist_v <- function(con) {
#   d <-
#     tbl_mar(con,"vessel.vessel_hist_v")
#   return(d)
# }

# v <- vessel_hist_v(con)
# v |> glimpse()
# 


# # support tables
# tbl_mar(con, "vessel.region") |> glimpse()
# tbl_mar(con, "vessel.usage_category") |> glimpse()
# tbl_mar(con, "vessel.vessel_operational") |> glimpse()
