#' Logbook catch by species
#'
#' @param con oracle connection
#'
#' @return sql query and data
#' @export
#'
lb_catch <- function(con) {
  q <-
    tbl_mar(con, "afli.afli") %>%
    dplyr::select(visir,
                  sid = tegund,
                  catch = afli)
  return(q)
}

lb_base0 <- function(con) {
  
  mar::afli_stofn(con) %>%
    dplyr::select(visir,
                  vid = skipnr,
                  gid = veidarf,
                  year = ar,
                  month = man,
                  date = vedags,
                  lon = lengd,
                  lat = breidd,
                  lon2 = lengd_lok,
                  lat2 = breidd_lok,
                  sq = reitur,        # Local statistical rectange (same resolution as ICES)
                  ssq = smareitur,    # The quarter within a rectangle. NOTE: It is derived if NA
                  z1 = dypi,          # depth
                  z2 = dypi_lok,
                  winddirection = vindatt,
                  beaufort = vindstig,
                  m_sec = m_sek,       # Meters per second??
                  distance = toglengd, # Derived measure
                  datel = ldags,       # Landing date
                  hid = lhofn,        # Harbour id landings took place
                  dplyr::everything())
  
}

#' Logbook base (stofn)
#' 
#' @param con oracle connection
#' @param correct_gear a boolean (default FALSE) checks for lookup-table for
#' gear correction (adds variable "gidc" to the tibble)
#'
#' @return sql query and data
#' @export
#'
lb_base <- function(con, correct_gear = FALSE) {
  
  q <-
    lb_base0(con)
  
  if(correct_gear) {
    q <- 
      q %>% 
      dplyr::left_join(gid_correction(con) %>% 
                         dplyr::select(visir, gidc),
                       by = "visir") %>% 
      dplyr::rename(gid_old = gid,
                    gid = gidc) %>% 
      # if gear correction not yet made in the oracle lookup table
      dplyr::mutate(gid = ifelse(is.na(gid), gid_old, gid)) %>% 
      dplyr::left_join(gid_orri_plus(con) %>%
                         dplyr::select(gid, gid2),
                       by = "gid")
  } else {
    q <- 
      q %>% 
      dplyr::left_join(gid_orri_plus(con) %>%
                         dplyr::select(gid, gid2),
                       by = "gid")
  }
  
  q <- 
    q %>% 
    dplyr::select(visir, gid, year:hid,
                  dplyr::everything())
  
  
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

lb_mobile <- function(con, correct_gear = TRUE, trim = TRUE) {
  
  q <- 
    lb_base(con, correct_gear = correct_gear) %>% 
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

#' Logbook static (passsive) gear
#'
#' @param con Oracle connection
#' @param correct_gear a boolean (default TRUE) checks for lookup-table for
#' gear correction (adds variable "gidc" to the tibble)
#' @param trim trim variables returned (default TRUE)
#' 
#' @return A sql tibble
#' @export
#' 
lb_static <- function(con, correct_gear = TRUE, trim = TRUE) {
  
  q <- 
    lb_base(con, correct_gear = correct_gear) %>% 
    dplyr::inner_join(tbl_mar(con, "afli.lineha"),
                      by = "visir")
  
  q <-
    q %>% 
    dplyr::mutate(effort = dplyr::case_when(gid2 == 1 ~ as.numeric(onglar * bjod),
                                            # netnights - the old measure used in iceland
                                            gid2 == 2 ~ as.numeric(dregin * naetur),
                                            # jigger hookhours
                                            gid2 == 3 ~ as.numeric(faeri * klst)),
                  effort_unit = dplyr::case_when(gid2 == 1 ~ "hooks",
                                                 gid2 == 2 ~ "netnights",
                                                 gid2 == 3 ~ "hookhours")) %>%
    dplyr::select(visir, vid, gid, year:hid,
                  effort,
                  effort_unit,
                  mesh = moskvi,           # gillnets
                  height = haed,           # gillnets
                  mean_gillnet_length = medal_lengd_neta,
                  bait = beita,
                  tempb1 = botnhiti,       # bottom temperature
                  temps1 = uppsjavarhiti,  # surface temperature
                  t0 = logn_hefst,         # time setting starts
                  t1 = drattur_hefst,      # time gear hauling starts
                  t2 = drattur_lykur,      # time gear hauling ends
                  # check what fj_kroka really is, looks like it is
                  # a "new" variable related to longline fishing
                  fj_kroka,
                  dplyr::everything())
  
  if(trim) {
    q <-
      q %>% 
      dplyr::select(visir:fj_kroka)
  }
  
  return(q)
  
}


#' Logbook traps
#'
#' @param con Oracle connection
#' @param correct_gear a boolean (default TRUE) checks for lookup-table for
#' gear correction (adds variable "gidc" to the tibble)
#' @param trim trim variables returned (default TRUE)
#' 
#' @return A sql tibble
#' @export
#' 
lb_trap <- function(con, correct_gear = TRUE, trim = TRUE) {
  
  q <- 
    lb_base(con, correct_gear = correct_gear) %>% 
    dplyr::inner_join(tbl_mar(con, "afli.gildra"),
                      by = "visir")
  
  q <-
    q %>% 
    dplyr::mutate(effort = gildrur * klst,
                  effort_unit = "traphours") %>%
    dplyr::select(visir, vid, gid, year:hid,
                  effort,
                  effort_unit,
                  dplyr::everything())
  
  if(trim) {
    q <-
      q %>% 
      dplyr::select(visir:effort_unit)
  }
  
  return(q)
  
}


#' Logbook pelagic seines
#'
#' @param con Oracle connection
#' @param correct_gear a boolean (default TRUE) checks for lookup-table for
#' gear correction (adds variable "gidc" to the tibble)
#' @param trim trim variables returned (default TRUE)
#' 
#' @return A sql tibble
#' @export
#' 
lb_seine <- function(con, correct_gear = TRUE, trim = TRUE) {
  
  q <- 
    lb_base(con, correct_gear = correct_gear) %>% 
    dplyr::inner_join(tbl_mar(con, "afli.hringn"),
                      by = "visir")
  
  q <-
    q %>% 
    dplyr::mutate(effort =  1,
                  effort_unit = "setting") |> 
    dplyr::mutate(on.bottom = lpad(klukkan, 4, "0")) %>%
    # vedags + (substr(lpad(ibotni,4,'0'),1,2)*60+substr(lpad(ibotni,4,'0'),3,2))/24/60 t1
    # Oracle time is in days
    dplyr::mutate(t1 = date + (substr(on.bottom, 1, 2) * 60 + substr(on.bottom, 3, 4)) / (24 * 60)) %>% 
    dplyr::select(visir, vid, gid, year:hid, 
                  effort,
                  effort_unit,
                  mesh = moskvi,
                  t1, 
                  dplyr::everything())
  
  if(trim) {
    q <-
      q %>% 
      dplyr::select(visir:t1)
  }
  
  return(q)
  
}



lb_std_meshsize <- function(d) {
  d %>%
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
      q %>% 
      dplyr::select(time = timi,
                    #lon = lengd,  these are useless values
                    #lat = breidd,
                    speed = skip_hradi,
                    heading = skip_stefna,
                    z = botndypi,
                    visir,
                    dplyr::everything()) |> 
      dplyr::mutate(speed = speed * 1.94384449)
  }
  if(std & trim) {
    q <-
      q |> 
      dplyr::select(time:visir) |> 
      dplyr::left_join(lb_base(con) |> 
                         dplyr::select(visir, vid),
                       by = "visir")
  }
  return(q)
}
  