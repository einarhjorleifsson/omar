# Official Icelandic vessel registry -------------------------------------------
#' The official Icelandic vessel registry
#' 
#' Access to table kvoti.skipaskra_siglo
#'
#' @param con oracle connection
#' @param standardize boolean (default TRUE), returns "standardized" variable names
#' @param trim boolean (default TRUE), returns only "standardized" variable names
#'
#' @return a query
#' @export
#'
vessel_registry <- function(con, standardize = TRUE, trim = TRUE) {
  
  q <-
    tbl_mar(con, "kvoti.skipaskra_siglo")
  
  if( standardize ) {
    q <-
      q %>%
      dplyr::select(vid = skipnr,
                    vessel = nafnskips,
                    uid = umdnr,
                    cs = kallmerki,
                    imo = imonr,
                    vclass = notkunarteg,
                    port = heimahofn,
                    propeller_diameter = thvermskrufu,
                    engine_kw = vel_kw,
                    power_index = aflvisir,
                    length_registered = skradlengd,
                    width = skradbreidd,
                    depth = skraddypt,
                    length = mestalengd,
                    brl = bruttoruml,    # neet a proper acronym
                    grt = bruttotonn,
                    dplyr::everything()) %>%
      # Need to double check ghost ships
      dplyr::mutate(length_registered = length_registered / 100,
                    # units of cm to meters
                    width = width / 100,
                    depth = depth / 100,
                    length = dplyr::case_when(vid == 9928 ~ 5,
                                              TRUE ~ length / 100),
                    #               "correct" brl for Asgrimur Halldorsson
                    brl = dplyr::case_when(vid == 2780 ~ brl / 100000,
                                           vid == 9928 ~ 2,
                                           TRUE ~ brl / 100),
                    grt = dplyr::case_when(vid == 9928 ~ 2,
                                           TRUE ~ grt / 100),
                    #                     Blidfari has abnormal engine_kw, divided by 100
                    engine_kw = dplyr::case_when(vid == 2069 ~ engine_kw / 10000,
                                                 TRUE ~ engine_kw / 100),
                    vessel = str_trim(vessel),
                    port = str_trim(port),
                    length_class = dplyr::case_when(length < 8 ~ "<8",
                                                    length >= 8  & length < 10 ~ "08-10",
                                                    length >= 10 & length < 12 ~ "10-12",
                                                    length >= 12 & length < 15 ~ "12-15",
                                                    length >= 15 ~ ">=15",
                                                    TRUE ~ NA_character_),
                    uid = str_trim(uid),
                    uid = dplyr::case_when(uid == "IS" ~ "ÍS",
                                           uid == "OF" ~ "ÓF",
                                           uid == "KÓ" ~ "KO",
                                           uid == "ZZ0" ~ "ZZ",      # Not valid but is also in skipaskra fiskistofu
                                           TRUE ~ uid),
                    uid = dplyr::case_when(nchar(uid) > 2 ~ paste0(str_sub(uid, 1, 2), "-", str_sub(uid, 3)),
                                           TRUE ~ uid),
                    cs = str_trim(cs),
                    cs = ifelse(nchar(cs) == 4 & str_sub(cs, 1, 2) == "TF",
                                cs,
                                NA_character_),
                    imo = ifelse(imo == 0, NA_integer_, imo),
                    vclass = as.integer(vclass))
  }
  
  if(trim) {
    q <- 
      q |> 
      dplyr::select(vid:grt, length_class)
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
#'
#' @return a query
#' @export
#'
vessel_history <- function(con) {
  tbl_mar(con, "kvoti.skipasaga") %>%
    dplyr::filter(skip_nr > 1) %>%
    dplyr::mutate(einknr = dplyr::case_when(nchar(einknr) == 1 ~ paste0("00", einknr),
                                            nchar(einknr) == 2 ~ paste0("0",  einknr),
                                            TRUE ~ as.character(einknr)),
                  einkst = paste0(einkst, einknr)) %>%
    dplyr::rename(vid = skip_nr, hist = saga_nr, t1 = i_gildi, t2 = ur_gildi,
                  uid = einkst, code = flokkur) %>%
    dplyr::left_join(tbl_mar(con, "kvoti.utg_fl") %>%
                       dplyr::select(code = flokkur, flokkur = heiti),
                     by = "code") %>%
    dplyr::select(-c(einknr, snn:sbn)) %>%
    dplyr::select(vid:code, flokkur, dplyr::everything()) |> 
    dplyr::arrange(vid, hist)
  
}

#' @rdname vessel_history
#' @export
vid_history <- vessel_history





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
    dplyr::rename(vclass = code)
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
  lh <- function(x, n) { str_sub(x, n, n) %>% as.integer() }
  #if(nchar(x) != 7) return(FALSE)
  x2 <- 
    lh(x, 1) * 7 +
    lh(x, 2) * 6 +
    lh(x, 3) * 5 +
    lh(x, 4) * 4 +
    lh(x, 5) * 3 +
    lh(x, 6) * 2
  return(str_sub(x2, nchar(x2)) == str_sub(x, nchar(x)))
}
