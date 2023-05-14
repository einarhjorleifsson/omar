stk_midvid <- function(con) {
  tbl_mar(con, "ops$einarhj.mobile_vid")
}

stk_trail0 <- function(con, VIDs) {
  
  if(missing(VIDs)) {
    q <- stk_midvid(con)
  } else {
    q <- 
      stk_midvid(con) |> 
      dplyr::filter(vid %in% VIDs)
  }
  
  q |> 
    dplyr::select(vid, mid, t1, t2) |> 
    dplyr::mutate(t1 = to_date(t1, "YYYY:MM:DD"),
                  t2 = to_date(t2, "YYYY:MM:DD")) |> 
    dplyr::left_join(stk_trail(con),
                     by = "mid") |> 
    dplyr::filter(time >= t1 & time <= t2)
  
}


#' The AIS/VMS trail
#'
#' @param con The connection
#' @param year An atomic value (only one year) to narrow time window
#'
#' @return a query
#' @export
#'
stk_trail <- function(con, year) {
  
  q <-
    tbl_mar(con, "stk.trail") %>%
    dplyr::mutate(lon = poslon * 180 / pi,
                  lat = poslat * 180 / pi,
                  heading = heading * 180 / pi,
                  speed = speed * 3600/1852)
  if(!missing(year)) {
    y1 <- paste0(year, "-01-01")
    y2 <- paste0(year + 1, "-01-01")
    q <-
      q %>%
      dplyr::filter(recdate >= to_date(y1, "YYYY:MM:DD"),
                    recdate   <  to_date(y2, "YYYY:MM:DD"))
  }
  
  q %>%
    dplyr::select(mid = mobileid,
                  time = posdate,
                  lon, lat, speed, heading,
                  hid = harborid,
                  io = in_out_of_harbor,
                  rectime = recdate)
  
}


#' Link vessel id and mobileid
#'
#' @param con The connection
#' @param correct Boolean, if FALSE (default) only returns stk.mobile as is
#' @param vidmatch Boolean, if FALSE (default) no matching attempted
#' @param classify Boolean, if FALSE (default) no classification attempted
#'
#' @return a query
#' @export
#'
stk_mobile_icelandic <- function(con, correct = FALSE, vidmatch = FALSE, classify = FALSE) {
  
  q <-
    tbl_mar(con, "stk.mobile") %>%
    dplyr::select(mid = mobileid, localid, globalid)
  
  if( correct ) {
    
    # NOTE: There are duplicate mid below, will only match "vessel"
    #       first in the list
    
    q <-
      q  %>%
      dplyr::mutate(localid_original = localid) %>%
      # Correcting icelandic vessels
      #   NOTE 2019-07-21 mid 133558 is old SISIMIUT now reflagged in Iceland
      #        mt states IMO 9039779, which is old vid 2173
      #
      dplyr::mutate(localid = dplyr::case_when(mid == 102869 ~ "6155",    # 2021-04-20: Hafró
                                               mid == 135587 ~ "2957",    # 2021-04-20: Páll Jónsson, nýtt skip 2020
                                               mid == 101065 ~ "1612",    # 2021-04-20: Hallgrímur: NOTE: THIS MID USED BEFORE
                                               mid == 101075 ~ "256",     # 2021-04-20: Kristrún II
                                               mid == 103067 ~ "962",     # 2021-04-20: Óskar
                                               mid == 103934 ~ "2962",    # 2021-04-20: Vörður, nýtt skip 2019
                                               mid == 103583 ~ "2966",    # 2021-04-20: Steinunn, nýtt skip 2019
                                               mid == 101161 ~ "2154",    # 2021-04-20: Árbakur
                                               mid == 100013 ~ "6643",    # 2020-03-10: Gimli, yfirskrifa skipnr 1156 sem datt af skra 2007                                               mid == 101069 ~ "1275",    # Jon Vidalí-in
                                               mid == 101078 ~ "2549",    # Thor HF-4 NOTE: DUBIOUS CORRECTION,
                                               mid == 101089 ~ "2907",    # 2020-03-10: Einar Gudnason, yfirskrifa skipnr 1012 datt ut af skra 2005
                                               mid == 101069 ~ "1275",    # Jon Vidalí-in
                                               mid == 101103 ~ "2954",    # 2020-03-10: Vestmannaey in nyja, var (2007) Vestmannaey gamla, vid = 1273
                                               mid == 101388 ~ "2961",    # 2020-03-10: Kristjan HF-100, yfirskrifa skipnr 2422 sem datta af skra 2007
                                               mid == 101429 ~ "1066",    # Aegir RE-0
                                               mid == 101439 ~ "2904",    # Pall Palsson IS-102 - NOTE: overwrote old vid
                                               mid == 101444 ~ "1421",    # Thyr RE-0
                                               mid == 101485 ~ "130",     # Bjarnarey VE-21
                                               mid == 101499 ~ "1809",    # JONA EÐVALDS II    SF208
                                               mid == 101545 ~ "2894",    # Bjorg EA-7  - NOTE: overwrote old vid
                                               mid == 101550 ~ "2908",    # 2020-03-10: Vesteinn GK 88, yfirskrifa 2377 sem datt ut 2377 sem datt ut 2003
                                               mid == 101558 ~ "2947",    # 2020-03-10: Indridi Kristins, yfirskrifa 980 sem datt af skra 2004
                                               mid == 101680 ~ "259",     # Jokull ÞH-259
                                               mid == 101775 ~ "1752",    # Brynjolfur VE-3
                                               mid == 101776 ~ "1512",    # Skarfur GK-666 ????
                                               mid == 101783 ~ "1413",    # Harpa VE-25
                                               mid == 101878 ~ "1578",    # Otto N Thorlaksson VE-5
                                               mid == 102100 ~ "1272",    # Sturla GK-12
                                               mid == 102101 ~ "2074",    # Baldur RE-0
                                               mid == 102284 ~ "2917",    # Solberg ÓF-1
                                               mid == 102497 ~ "2787",    # Andrea AK-0
                                               mid == 102515 ~ "2643",    # Jupoter ÞH-363
                                               mid == 102543 ~ "2706",    # 2020-02-10: replace globalid = GK299, only active in 2008
                                               mid == 102561 ~ "2895",    # Videy RE-50
                                               mid == 102571 ~ "1281",    # Mulaberg
                                               mid == 102795 ~ "2848",    # Ambassador, passenger vessel
                                               mid == 102817 ~ "-----",   # Utlenskt skip, liklega adur 1552 Mar SH-127 imo: 7827732
                                               mid == 102965 ~ "1337",    # Skafti HF-48
                                               mid == 103015 ~ "2702",    # Gandí VE-171
                                               mid == 103251 ~ "2948",    # Barkur
                                               mid == 103765 ~ "997",     # Hvalur 9 RE-399
                                               mid == 103772 ~ "2940",    # Hafborg EA-152
                                               mid == 107645 ~ "1937",    # Bjorgvin EA-311
                                               mid == 121166 ~ "2906",    # Dagur SK-17
                                               mid == 127224 ~ "2890",    # Akurey AK-10
                                               mid == 127288 ~ "-----",   # Eitthvurt utlenskt skip, ekki vid = 2276
                                               mid == 133558 ~ "2173",    # Tomas Thorvaldsson GK-10 - gamli Sisimiut
                                               mid == 135534 ~ "2965",    # Bardur SH 81, nyskradur 2019
                                               TRUE ~ localid))
  }
  
  if( vidmatch ) {
    
    # There must be a neater way :-)
    cn1 <- as.character(1:1000)
    cn2 <- as.character(1001:2000)
    cn3 <- as.character(2001:3000)
    cn4 <- as.character(3001:3772)
    cn6 <- as.character(5001:6000)
    cn7 <- as.character(6001:7000)
    cn8 <- as.character(7001:8000)
    cn9 <- as.character(8001:9000)
    cn10 <- as.character(9001:9997)
    q <-
      q %>%
      dplyr::mutate(vid =  dplyr::case_when(localid %in% cn1 ~ localid,
                                            localid %in% cn2 ~ localid,
                                            localid %in% cn3 ~ localid,
                                            localid %in% cn4 ~ localid,
                                            localid %in% cn6 ~ localid,
                                            localid %in% cn7 ~ localid,
                                            localid %in% cn8 ~ localid,
                                            localid %in% cn9 ~ localid,
                                            localid %in% cn10 ~ localid,
                                            TRUE ~ NA_character_)) %>%
      dplyr::mutate(vid2 = dplyr::case_when(globalid %in% cn1 ~ globalid,
                                            globalid %in% cn2 ~ globalid,
                                            globalid %in% cn3 ~ globalid,
                                            globalid %in% cn4 ~ globalid,
                                            globalid %in% cn6 ~ globalid,
                                            globalid %in% cn7 ~ globalid,
                                            globalid %in% cn8 ~ globalid,
                                            globalid %in% cn9 ~ globalid,
                                            globalid %in% cn10 ~ globalid,
                                            TRUE ~ NA_character_)) %>%
      dplyr::mutate(vid = as.integer(vid),
                    vid2 = as.integer(vid2),
                    vid = ifelse(is.na(vid), vid2, vid)) %>%
      dplyr::select(-vid2)
  }
  
  
  if( classify ) {
    
    q <-
      q %>%
      dplyr::left_join(vid_mmsi(con) %>%
                         dplyr::select(vid, mmsi) %>%
                         dplyr::filter(!is.na(vid)),
                       by = "vid")
    q <-
      q %>%
      dplyr::mutate(bauja = ifelse(toupper(stringr::str_sub(globalid, 5, 9)) %in% c("_NET_", "_NET1", "_NET2", "_NET3", "_NET4"),
                                   "hi",
                                   NA)) %>%
      # NOTE Code for just finding numericals in the global string since below NOT 100 foolproof
      dplyr::mutate(gid_temp = stringr::str_trim(globalid)) %>%
      dplyr::mutate(mmsi = dplyr::case_when(!is.na(mmsi) ~ mmsi,
                                            nchar(stringr::str_trim(gid_temp)) == 9 &
                                              stringr::str_sub(gid_temp, 1, 1) %in% c("0", "1", "2", "3", "4", "5", "6", "7", "8", "9") &
                                              is.na(bauja) ~ gid_temp,
                                            TRUE ~ NA_character_)) %>%
      dplyr::select(-gid_temp)
    # mutate(mmsi = ifelse(nchar(stringr::str_trim(globalid)) == 9 &
    #                        stringr::str_sub(globalid, 1, 1) %in% c("0", "1", "2", "3", "4", "5", "6", "7", "8", "9") &
    #                        is.na(bauja),
    #                      stringr::str_trim(globalid),
    #                      NA_character_))
    
    
    q <-
      q %>%
      dplyr::mutate(mmsi.mid = dplyr::case_when(as.integer(stringr::str_sub(mmsi, 1, 1)) %in% c("2", "3", "4", "5", "6", "7") ~ stringr::str_sub(mmsi, 1, 3),
                                                str_sub(mmsi, 1, 2) %in% c("98", "99") ~ stringr::str_sub(mmsi, 3, 5),
                                                TRUE ~ NA_character_))
    
    q <-
      q %>%
      dplyr::left_join(vessel_mid(con) %>%
                         dplyr::select(mmsi.mid = mid, mid_iso2 = iso2),
                       by = "mmsi.mid")
    
    q <-
      q %>%
      dplyr::mutate(class = dplyr::case_when(stringr::str_sub(mmsi, 1, 1) == "1" ~ "sar aircraft",
                                             stringr::str_sub(mmsi, 1, 1) %in% c("2", "3", "4", "5", "6", "7") ~ "vessel",
                                             stringr::str_sub(mmsi, 1, 1) == "8" ~ "handheld VHF transceiver",
                                             stringr::str_sub(mmsi, 1, 2) == "99" ~ "bauja",
                                             stringr::str_sub(mmsi, 1, 2) == "98" ~ "sibling craft",
                                             stringr::str_sub(mmsi, 1, 3) == "970" ~ "sar transponders",
                                             stringr::str_sub(mmsi, 1, 3) == "972" ~ "man overboard",
                                             !is.na(mmsi.mid) & is.na(as.integer(stringr::str_sub(mmsi, 1, 2))) ~ "vessel",
                                             localid_original %in% c("9999", "9998") |
                                               globalid %in% c("Surtseyja", "Straumnes", "Steinanes", "Haganes_K", "Eyri_Kvi_", "Bakkafjar",
                                                               "Laugardal", "BorgfjE P", "Gemlufall", "Sjokvi", "Straumduf", "Eyrarhlid",
                                                               "Hvalnes", "Straumm?l", "V_Blafj P", "Isafj.dju", "Rey?arfjo",
                                                               "VidarfjAI", "KLIF AIS",  "VadlahAIS", "Hafell_AI", "TIND_AIS",  "Storhof?i",
                                                               "Helguv", "Laugarb.f", "Grimseyja", "G.skagi",   "Grindavik", "Hornafjar",
                                                               "Flateyjar", "Kogurdufl", "Blakkur.d", "Bakkafjor", "Hvalbakur", "SUGANDI_A",
                                                               "TJALDANES", "Sjokvi-4",  "Kvi-0 Hri", "Sjokvi-2", "Sjokvi-3", "Snaefj1",
                                                               "Snaefj2", "Lande", "Sjomsk", "TJALD.NES", "illvid_p", "BLAKKSNES", "V_Sfell B",
                                                               "HOF", "118084", "Illvi?rah", "Miðfegg P", "BASE11", "Borgarfj ",
                                                               "V_Hofsos", "V_Hofsos ", "Arnarfjor", "Trackw", "SUGANDAFJ",
                                                               "BORGARÍS", "BORGARIS", "BORGARIS0", "BORGARIS1",
                                                               "TEST", "SN-105717") ~ "fixed",
                                             !is.na(bauja) ~ "bauja",
                                             !is.na(vid) ~ "vessel",
                                             mid %in% c(102817, 127288) ~ "vessel",
                                             mid %in% c(118084, 103135) ~ "fixed",
                                             TRUE ~ NA_character_))
    
    # NOTE: CHECK HERE
    q <-
      q %>%
      dplyr::mutate(cs = ifelse(class == "vessel" | is.na(class), globalid, NA_character_),
                    cs = dplyr::case_when(cs == "THAE" & vid == 2549 ~ "TFAE",
                                          stringr::str_sub(cs, 1, 4) == "TMP_" ~  NA_character_,
                                          TRUE ~ cs),
                    cs_prefix = stringr::str_sub(cs, 1, 2)) %>%
      dplyr::left_join(vessel_csprefix(con) %>% dplyr::select(cs_prefix, cs_iso2 = iso2),
                       by = "cs_prefix")  %>%
      dplyr::mutate(cs = ifelse(!is.na(cs_iso2), cs, NA_character_))
    
    # NOTE: This may have to be put upstream
    #q <-
    #  q %>%
    #  mutate(vid = ifelse(!between(vid, 3700, 4999) & cs_iso2 != "IS", NA, vid))
    q <-
      q %>%
      dplyr::mutate(vid.aux = ifelse(class == "bauja" & is.na(cs), stringr::str_sub(globalid, 1, 4), NA_character_)) %>%
      dplyr::select(-c(bauja, mmsi.mid))
  }
  
  
  return(q)
  
}

