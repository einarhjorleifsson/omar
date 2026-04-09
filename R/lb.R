#' Logbook catch by species
#'
#' @param con oracle connection
#'
#' @return sql query and data
#' @export
#'
lb_catch <- function(con) {
  q <-
    tbl_mar(con, "afli.afli") |> 
    translate_names(dictionary) |> 
    dplyr::select(.sid, sid, catch)
  return(q)
}


#' Logbook base (stofn)
#' 
#' @param con oracle connection
#'
#' @return sql query and data
#' @export
#'
lb_base <- function(con) {
  
  q <-
    mar::afli_stofn(con) |> 
    translate_names(dictionary) |> 
    dplyr::select(.sid,
                  vid,
                  gid,
                  year,
                  month,
                  date,
                  lon1,
                  lat1,
                  lon2,
                  lat2,
                  sq,
                  ssq,
                  z1, z2,
                  winddirection,
                  beaufort,
                  m_sec,       # Meters per second??
                  distance, # Derived measure
                  D2,       # Landing date
                  hid2,        # Harbour id landings took place
                  dplyr::everything())
  
  return(q)
}


#' Logbook mobile (active) gear
#'
#' @param con oracle connection
#' @param trim trim variables returned (default TRUE)
#'
#' @return a sql tibble
#' @export

lb_mobile <- function(con, trim = TRUE) {

  q <-
    lb_base(con) |>
    dplyr::filter(gid %in% c(6, 7, 8, 9, 14, 15, 38, 40, 5, 18, 26, 39)) |>
    dplyr::inner_join(tbl_mar(con, "afli.toga") |>
                        translate_names(dictionary),
                      by = ".sid")

  q <-
    q |>
    dplyr::mutate(effort = dplyr::case_when(gid %in% c(6, 7, 8, 9, 14, 15, 38, 40) ~ towtime / 60,
                                            # for seine and traps use setting as effort
                                            gid %in% c(5, 18, 26, 39) ~ 1,
                                            TRUE ~ NA_real_),
                  effort_unit = dplyr::case_when(gid %in% c(6, 7, 8, 9, 14, 15, 38, 40) ~ "hours towed",
                                                 # for seine just use the setting
                                                 gid %in% c(5, 18, 26, 39) ~ "setting",
                                                 TRUE ~ NA_character_)) |>
    dplyr::mutate(on.bottom = dplyr::case_when(on.bottom < 0 ~ NA,
                                               .default = on.bottom),
                  on.bottom = lpad(on.bottom, 4, "0")) |>
    # vedags + (substr(lpad(ibotni,4,'0'),1,2)*60+substr(lpad(ibotni,4,'0'),3,2))/24/60 t1
    # Oracle time is in days
    dplyr::mutate(t1 = date + (substr(on.bottom, 1, 2) * 60 + substr(on.bottom, 3, 4)) / (24 * 60),
                  t2 = date + (substr(on.bottom, 1, 2) * 60 + substr(on.bottom, 3, 4) + towtime) / (24 * 60)) |>
    dplyr::select(.sid, vid, gid, year:hid2,
                  towtime,                     # in minutes
                  effort,
                  effort_unit,
                  mesh,
                  mesh_min,
                  doors,              # in kilograms
                  headline,
                  sweeps,          # in meters ???
                  plow_width,
                  tempb1,           # bottom temperature
                  tempb2,
                  temps1,         # surface temperature
                  temps2,
                  t1,
                  t2,
                  on.bottom,
                  dplyr::everything())

  if(trim) {
    q <-
      q |>
      dplyr::select(.sid:on.bottom)
  }

  return(q)
}

#' Logbook static (passsive) gear
#'
#' @param con Oracle connection
#' @param trim trim variables returned (default TRUE)
#' 
#' @return A sql tibble
#' @export
#' 
lb_static <- function(con, trim = TRUE) {

  q <-
    lb_base(con) |>
    dplyr::filter(gid %in% c(1, 16, 32, 36, 71, 2, 11, 25, 29, 72, 90, 91, 92, 3)) |>
    dplyr::inner_join(tbl_mar(con, "afli.lineha") |>
                        translate_names(dictionary),
                      by = ".sid")

  q <-
    q |>
    dplyr::mutate(effort = dplyr::case_when(gid %in% c(1, 16, 32, 36, 71) ~ as.numeric(onglar * bjod),
                                            # netnights - the old measure used in iceland
                                            gid %in% c(2, 11, 25, 29, 72, 90, 91, 92) ~ as.numeric(dregin * naetur),
                                            # jigger hookhours
                                            gid == 3 ~ as.numeric(faeri * klst)),
                  effort_unit = dplyr::case_when(gid %in% c(1, 16, 32, 36, 71) ~ "hooks",
                                                 gid %in% c(2, 11, 25, 29, 72, 90, 91, 92) ~ "netnights",
                                                 gid == 3 ~ "hookhours")) |>
    dplyr::select(.sid, vid, gid, year:hid2,
                  effort,
                  effort_unit,
                  mesh,           # gillnets
                  height,           # gillnets
                  mean_gillnet_length,
                  bait,
                  tempb1,       # bottom temperature
                  temps1,  # surface temperature
                  t0,         # time setting starts
                  t1,      # time gear hauling starts
                  t2,      # time gear hauling ends
                  # check what fj_kroka really is, looks like it is
                  # a "new" variable related to longline fishing
                  fj_kroka,
                  dplyr::everything())

  if(trim) {
    q <-
      q |>
      dplyr::select(.sid:fj_kroka)
  }

  return(q)

}


#' Logbook traps
#'
#' @param con Oracle connection
#' @param trim trim variables returned (default TRUE)
#' 
#' @return A sql tibble
#' @export
#' 
lb_trap <- function(con, trim = TRUE) {
  
  q <- 
    lb_base(con) |> 
    dplyr::filter(gid %in% c(17, 18, 39)) |> 
    dplyr::inner_join(tbl_mar(con, "afli.gildra") |> 
                        translate_names(dictionary),
                      by = ".sid")
  
  q <-
    q |> 
    dplyr::mutate(effort = n_units * hours,
                  effort_unit = "traphours") |>
    dplyr::select(.sid, vid, gid, year:hid2,
                  effort,
                  effort_unit,
                  dplyr::everything())
  
  if(trim) {
    q <-
      q |> 
      dplyr::select(.sid:effort_unit)
  }
  
  return(q)
  
}


#' Logbook pelagic seines
#'
#' @param con Oracle connection
#' @param trim trim variables returned (default TRUE)
#' 
#' @return A sql tibble
#' @export
#' 
lb_seine <- function(con, trim = TRUE) {
  
  q <- 
    lb_base(con) |> 
    dplyr::filter(gid %in% c(10, 12)) |> 
    dplyr::inner_join(tbl_mar(con, "afli.hringn") |> 
                        translate_names(dictionary),
                      by = ".sid")
  
  q <-
    q |> 
    dplyr::mutate(effort =  1,
                  effort_unit = "setting") |> 
    dplyr::mutate(on.bottom = lpad(klukkan, 4, "0")) |>
    # vedags + (substr(lpad(ibotni,4,'0'),1,2)*60+substr(lpad(ibotni,4,'0'),3,2))/24/60 t1
    # Oracle time is in days
    dplyr::mutate(t1 = date + (substr(on.bottom, 1, 2) * 60 + substr(on.bottom, 3, 4)) / (24 * 60)) |> 
    dplyr::select(.sid, vid, gid, year:hid2, 
                  effort,
                  effort_unit,
                  mesh,
                  t1, 
                  dplyr::everything())
  
  if(trim) {
    q <-
      q |> 
      dplyr::select(.sid:t1)
  }
  
  return(q)
  
}



lb_std_meshsize <- function(d) {
  d |>
    dplyr::mutate(mesh.std = dplyr::case_when(gid == 2 ~ 0L,
                                              gid ==  9 ~ 80L,
                                              gid %in% c(7, 10, 12, 14) ~ 40L,
                                              gid %in% c(5, 6) & (mesh <= 147 | is.na(mesh)) ~ 135L,
                                              gid %in% c(5, 6) &  mesh >  147 ~ 155L,
                                              gid %in% c(15, 38, 40) ~ 100L,
                                              TRUE ~ NA_integer_))
  
}

#' Logbook trails
#'
#' @param con Oracle connection
#' @param std Boolean (default TRUE)
#' @param trim Boolean (default TRUE)
#'
#' @return A query
#' @export
#'
lb_trail <- function(con, std = TRUE, trim = TRUE) {
  q <- omar::tbl_mar(con, "afli.sjalfvirkir_maelar")
  
  if(std) {
    q <- 
      q |> 
      dplyr::select(time = timi,
                    #lon = lengd,  these are useless values
                    #lat = breidd,
                    speed = skip_hradi,
                    heading = skip_stefna,
                    z = botndypi,
                    .sid = visir,
                    dplyr::everything()) |> 
      dplyr::mutate(speed = speed * 1.94384449)
  }
  if(std & trim) {
    q <-
      q |> 
      dplyr::select(time:.sid) |> 
      dplyr::left_join(lb_base(con) |> 
                         dplyr::select(.sid, vid),
                       by = ".sid")
  }
  return(q)
}
