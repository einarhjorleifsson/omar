#' Title
#'
#' @param con oracle connection
#'
#' @return query
#' @export
#'
vessel_channel <- function(con) {
  tbl_mar(con, "channel.vessel") |> 
    select(vid = vessel_no,
           vessel = name,
           uid = identification,
           uno = identification_no,
           gt = gross_registered_ton,
           mclass = category_no)
}


# Official Icelandic vessel registry -------------------------------------------
#' The official Icelandic vessel registry
#' 
#' Access to table kvoti.skipaskra_siglo
#' @param con oracle connection
#' @param standardize boolean (default TRUE), returns "standardized" variable names
#' @param trim boolean (default TRUE), returns only "standardized" variable names
#'
#' @return a query
#' @export
#'
#'
# siglo |> 
#   select(vid, uid_o) |> 
#   mutate(uid_o = str_replace(uid_o, "\\.", ""),
#          unr = str_sub(uid_o, 3),#'
# 
#          uid = str_sub(uid_o, 1, 2)) |> 
#   mutate(unr2 = as.integer(unr),
#          unr2 = ifelse(unr2 == 0, NA, unr2))

vessel_registry <- function(con, standardize = TRUE, trim = TRUE) {
  
  q <-
    tbl_mar(con, "kvoti.skipaskra_siglo")
  
  if( standardize ) {
    q <-
      q %>%
      dplyr::select(vid = skipnr,
                    vessel = nafnskips,
                    uid = umdnr,
                    length = mestalengd,
                    length_reg = skradlengd,
                    gv = bruttoruml,    # neet a proper acronym
                    gt = bruttotonn,
                    vclass = notkunarteg,
                    width_reg = skradbreidd,
                    depth_reg = skraddypt,
                    cs = kallmerki,
                    imo = imonr,
                    port = heimahofn,   # Note: should really have hid here
                    propeller_diameter = thvermskrufu,
                    engine_kw = vel_kw,
                    power_index = aflvisir,
                    dplyr::everything()) %>%
      # Need to double check ghost ships
      dplyr::mutate(vessel = stringr::str_trim(vessel),
                    uid = stringr::str_trim(uid),
                    uno = stringr::str_sub(uid, 3),
                    uno = ifelse(uno %in% c("0", "00", "000", "."), NA, uno),
                    uno = as.numeric(uno),
                    uid = stringr::str_sub(uid, 1, 2),
                    uid = dplyr::case_when(uid == "IS" ~ "ÍS",
                                           uid == "OF" ~ "ÓF",
                                           uid == "KÓ" ~ "KO",
                                           TRUE ~ uid),
                    length = dplyr::case_when(vid == 9928 ~ 5,
                                              length == 0 ~ NA,
                                              TRUE ~ length / 100),
                    length_reg = dplyr::case_when(length_reg == 0 ~ NA,
                                           TRUE ~ length_reg / 100),
                    gv = dplyr::case_when(vid == 2780 ~ gv / 100000,
                                          vid == 9928 ~ 2,
                                          gv == 0 ~ NA_real_,
                                          TRUE ~ gv / 100),
                    #               "correct" brl for Asgrimur Halldorsson
                    gt = dplyr::case_when(vid == 9928 ~ 2,
                                          gt == 0 ~ NA_real_,
                                          TRUE ~ gt / 100),
                    # units of cm to meters
                    width_reg = dplyr::case_when(width_reg == 0 ~ NA,
                                                 TRUE ~ width_reg / 100),
                    depth_reg = dplyr::case_when(depth_reg == 0 ~ NA,
                                                 TRUE ~ depth_reg / 100),
                    cs = dplyr::case_when(nchar(cs) == 4 & stringr::str_sub(cs, 1, 2) == "TF" ~ cs,
                                          TRUE ~ NA_character_),
                    imo = ifelse(imo == 0, NA_integer_, imo),
                    #                     Blidfari has abnormal engine_kw, divided by 100
                    engine_kw = dplyr::case_when(vid == 2069 ~ engine_kw / 10000,
                                                 TRUE ~ engine_kw / 100),
                    engine_kw = ifelse(engine_kw == 0, NA, engine_kw),
                    power_index = ifelse(power_index == 0, NA, power_index),
                    propeller_diameter = ifelse(propeller_diameter == 0, NA, propeller_diameter),
                    port = stringr::str_trim(port),
                    length_class = dplyr::case_when(length < 8 ~ "<8",
                                                    length >= 8  & length < 10 ~ "08-10",
                                                    length >= 10 & length < 12 ~ "10-12",
                                                    length >= 12 & length < 15 ~ "12-15",
                                                    length >= 15 ~ ">=15",
                                                    TRUE ~ NA_character_),
                    vclass = as.integer(vclass))
  }
  
  if(trim) {
    q <- 
      q |> 
      dplyr::select(vid, vessel, uid, uno, length:power_index, length_class)
  }
  
  return(q)
  
}

#' @rdname vessel_registry
#' @export
vid_registry <- vessel_registry

# Vessel registry of the Directory of fisheries --------------------------------
#  Includes additional information on Icelandic vessels and also registry of
#  foreign vessels that have landed or fished in Icelandic waters

# tbl_mar(con, "orri.skipaskra") |> 
#   select(vid = skip_nr,
#          vessel = heiti,
#          ust = einkst,
#          uid = einknr,
#          class = flokkur)


# Vessel history by the Directory of fisheries ---------------------------------
#' Vessel history per recorded by Fiskistofa
#'
#' @param con oracle connection
#' @param trim Boolean (default TRUE) returning only limited variables
#'
#' @return a query
#' @export
#'
vessel_history <- function(con, trim = TRUE) {
  q <-
    tbl_mar(con, "kvoti.skipasaga") %>% 
    dplyr::filter(skip_nr > 4) |> 
    dplyr::select(vid = skip_nr,
                  vessel = heiti,
                  uid = einkst,
                  uno = einknr,
                  mclass = flokkur,
                  length = lengd,
                  gv = brl,
                  gt = bruttotonn,
                  length_reg = skrad_lengd,
                  width_reg = skrad_breidd,
                  depth_reg = skrad_dypt,
                  deck = dekk,
                  hid = heimah,
                  histno = saga_nr,
                  t1 = i_gildi,
                  t2 = ur_gildi,
                  owner = eigandi,
                  operator = rek_adili,
                  phone = simi,
                  comment = aths,
                  clarification = skyring,
                  dplyr::everything()) |> 
    dplyr::mutate(foreign = dplyr::case_when(between(vid, 3700, 4999) ~ 1,
                                             TRUE ~ 0),
                  length = ifelse(length <= 1, NA, length),
                  length_reg = ifelse(length_reg <= 1, NA, length_reg),
                  width_reg = ifelse(width_reg <= 0, NA, width_reg),
                  depth_reg = ifelse(depth_reg <= 0, NA, depth_reg),
                  # foreign vessels
                  uno = ifelse(foreign == 1, NA, uno),
                  uid = ifelse(uid %in% c("??", "X") & foreign == 1, NA, uid),
                  uid = stringr::str_to_upper(uid),
                  uid = dplyr::case_when(uid == "UK" & foreign == 1 ~ "GB", 
                                         uid == "PO" & foreign == 1 ~ "PL",
                                         TRUE ~ uid),
                  gv = ifelse(gv == 0 |
                                (gv < 1.0001 & is.na(length)) |
                                (gv < 1.0001 & length > 15),
                              NA_real_,
                              gv),
                  gt = ifelse(gt == 0 |
                                (gt < 1.0001 & is.na(length)) |
                                (gt < 1.0001 & length > 15),
                              NA_real_,
                              gt))
  
  if(trim) {
    q <- q |> dplyr::select(vid:t2, foreign)
  }
  
  return(q)
}

#' @rdname vessel_history
#' @export
vid_history <- vessel_history

#' Vessel management class
#'
#' @param con oracle connection
#' @param trim Boolean (default TRUE) returning only limited variables
#'
#' @return A query
#' @export
#'
vessel_mclass <- function(con, trim = TRUE) {
  tbl_mar(con, "kvoti.utg_fl") %>%
    dplyr::select(mclass = flokkur,
                  mname = heiti,
                  msystem = veidikerfi,
                  menglish = enskt_heiti)
  if(trim) {
    q <- q |> dplyr::select(mclass, mname)
  }
  return(q)
}



#' Icelandic vessel local identification letters
#' 
#' Table containing local identification letters and location name
#'
#' @param con sql connection
#'
#' @return sql query
#' @export
#'
vessel_ust <- function(con) {
  tbl_mar(con, "ops$einarhj.VESSEL_UMDAEMISBOKSTAFIR")
}






#' Vessel callsign country prefixes
#'
#' @param con sql connection
#'
#' @return a query
#' @source \url{https://en.wikipedia.org/wiki/ITU_prefix#Allocation_table}
#' @export
#'
vessel_cs_itu_prefix <- function(con) {
  tbl_mar(con, "ops$einarhj.VESSEL_CS_ITU_PREFIX")
}


#' The mother of all vessels
#'
#' Data from Samgongustofa, Directorates of Fisheries and some from MMSI registry
#'
#' @param con oracle connection
#'
#' @return A query
#' @export
#'
vessels_vessels <- function(con) {
  tbl_mar(con, "ops$einarhj.VESSELS")
}



#' Vessel class
#'
#' @param con oracle connection
#'
#' @return a query
#' @export
#'
vessel_class <- function(con) {
  tbl_mar(con, "ops$einarhj.VESSEL_CLASS") |> 
    dplyr::rename(vclass = code,
                  skipaflokkur = flokkur,
                  vesselclass = class) |> 
    dplyr::mutate(skipaflokkur = toupper(skipaflokkur),
                  skipaflokkur = ifelse(skipaflokkur == "UNSPECIFIED",
                                        "ÓTILGREINT",
                                        skipaflokkur),
                  vesselclass  = toupper(vesselclass))
}

#' Check if IMO is valid 
#'
#' @param x a numerical vector
#'
#' @return A boolean TRUE/FALSE vector
#' 
#' @source \url{https://en.wikipedia.org/wiki/IMO_number}
#' 
#' @export
#'
vessel_valid_imo <- function(x) {
  lh <- function(x, n) { stringr::str_sub(x, n, n) %>% as.integer() }
  #if(nchar(x) != 7) return(FALSE)
  x2 <- 
    lh(x, 1) * 7 +
    lh(x, 2) * 6 +
    lh(x, 3) * 5 +
    lh(x, 4) * 4 +
    lh(x, 5) * 3 +
    lh(x, 6) * 2
  return(stringr::str_sub(x2, nchar(x2)) == stringr::str_sub(x, nchar(x)))
}


#' Title
#'
#' @param con Oracle connection of a data frame
#' @param MID mobileid
#'
#' @return q mapdeck object
#' @export
#'
stk_mapdeck <- function (con, MID) {
  if(any(class(con) %in% "OraConnection")) {
    d <- stk_trail(con) %>% dplyr::filter(mid %in% MID) %>% dplyr::collect(n = Inf) 
  } else {
    d <- con
  }
  d %>% 
    dplyr::mutate(speed = ifelse(speed > 10, 10, speed)) %>% 
    mapdeck::mapdeck() %>% mapdeck::add_scatterplot(lon = "lon", 
                                                    lat = "lat", fill_colour = "speed", layer_id = "track", 
                                                    palette = "inferno")
}
