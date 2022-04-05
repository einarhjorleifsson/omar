# Umdæmisbókstafir íslenskra skipa ---------------------------------------------
# ust <-
#   tribble(~UST, ~STADUR,
#           "AK", "Akranes",
#           "NS", "Norður-Múlasýsla og Seyðisfjörður",
#           "ÁR", "Árnessýsla",
#           "ÓF", "Ólafsfjörður",
#           "BA", "Barðastrandarsýsla",
#           "RE", "Reykjavík",
#           "DA", "Dalasýsla",
#           "SF", "Austur-Skaftafellssýsla",
#           "EA", "Eyjafjarðarsýsla og Akureyri",
#           "SH", "Snæfellsness-og Hnappadalssýsla",
#           "GK", "Gullbringusýsla",
#           "SI", "Siglufjörður",
#           "HF", "Kjósarsýsla og Hafnarfjörður",
#           "SK", "Skagafjarðarsýsla og Sauðárkrókur",
#           "HU", "Húnavatnssýsla",
#           "ST", "Strandasýsla",
#           "ÍS", "Ísafjarðarsýsla",
#           "SU", "Suður-Múlasýsla",
#           "KE", "Keflavík",
#           "VE", "Vestmannaeyjar",
#           "KÓ", "Kópavogur",
#           "VS", "Vestur-Skaftafellssýsla",
#           "MB", "Mýra-og Borgarfjarðarsýsla",
#           "ÞH", "Þingeyjarsýslur",
#           "NK", "Neskaupstaður")
# dbWriteTable(con, name = "VESSEL_UMDAEMISBOKSTAFIR", value = ust, overwrite = TRUE)

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






#' The official vessel registry
#'
#' @param con oracle connection
#' @param standardize boolean (default TRUE), returns "standardized" variable names
#'
#' @return a query
#' @export
#'

vid_registry <- function(con, standardize = TRUE) {
  
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
                    grt = bruttotonn) %>%
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
  
  return(q)
  
}

vid_history <- function(con) {
  tbl_mar(con, "kvoti.skipasaga") %>%
    dplyr::filter(skip_nr > 1) %>%
    dplyr::mutate(einknr = dplyr::case_when(nchar(einknr) == 1 ~ paste0("00", einknr),
                                            nchar(einknr) == 2 ~ paste0("0",  einknr),
                                            TRUE ~ as.character(einknr)),
                  einkst = paste0(einkst, einknr)) %>%
    dplyr::rename(vid = skip_nr, hist = saga_nr, t1 = i_gildi, t2 = ur_gildi,
                  uid = einkst, code = flokkur) %>%
    dplyr::left_join(tbl_mar(con, "kvoti.utg_fl") %>%
                       dplyr::select(code = flokkur, flokkur = heiti)) %>%
    dplyr::select(-c(einknr, snn:sbn)) %>%
    dplyr::arrange(vid, hist) %>%
    dplyr::select(vid:code, flokkur, dplyr::everything())
  
}

# pth <- "https://www.pfs.is/library/Skrar/Tidnir-og-taekni/Numeramal/MMSI/NUMER%20Query270619.xlsx"
# download.file(pth, destfile = "data-raw/ss-270619_mmsi.xlsx")
# v_mmsi <-
#   readxl::read_excel("data-raw/ss-270619_mmsi.xlsx") %>%
#   janitor::clean_names() %>%
#   dplyr::select(SKNR = sknr,
#                 NAME = skip,
#                 CS = kallm,
#                 MMSI = mmsi_nr,
#                 STDC = standard_c) %>%
#   dplyr::mutate(VID = case_when(str_sub(MMSI, 1, 3) == "251" ~ as.integer(SKNR),
#                                 TRUE ~ NA_integer_),
#                 VID2 = case_when(str_sub(MMSI, 1, 3) != "251" ~ as.integer(SKNR),
#                                  TRUE ~ NA_integer_)) %>%
#   dplyr::select(SKNR, VID, VID2, NAME, MMSI, CS) %>%
#   dplyr::arrange(VID)
# dbWriteTable(con, name = "VESSEL_MMSI_20190627", value = v_mmsi, overwrite = TRUE)

# # updated table 2021-01-06
# pth <- "https://www.pfs.is/library/Skrar/Tidnir-og-taekni/Numeramal/MMSI/NUMER%20Query151220.xlsx"
# download.file(pth, destfile = "tmp.xlsx")
# v_mmsi <-
#   readxl::read_excel("tmp.xlsx") %>%
#   janitor::clean_names() %>%
#   dplyr::select(SKNR = sknr,
#                 NAME = skip,
#                 CS = kallm,
#                 MMSI = mmsi_nr,
#                 STDC = standard_c) %>%
#   dplyr::mutate(VID = dplyr::case_when(stringr::str_sub(MMSI, 1, 3) == "251" ~ as.integer(SKNR),
#                                        TRUE ~ NA_integer_),
#                 VID2 = dplyr::case_when(stringr::str_sub(MMSI, 1, 3) != "251" ~ as.integer(SKNR),
#                                         TRUE ~ NA_integer_)) %>%
#   dplyr::select(SKNR, VID, VID2, NAME, MMSI, CS) %>%
#   dplyr::arrange(VID)
# DBI::dbWriteTable(con, name = "VESSEL_MMSI_20201215", value = v_mmsi)

vid_mmsi <- function(con) {
  tbl_mar(con, "ops$einarhj.VESSEL_MMSI_20201215")
}

# ------------------------------------------------------------------------------
# MMSI country code
# library(rvest)
# library(countrycode)
# library(tidyverse)
# url <- "https://en.wikipedia.org/wiki/Maritime_identification_digits"
# mid <-
#   url %>%
#   read_html() %>%
#   #html_nodes(xpath='//*[@id="mw-content-text"]/table[1]') %>%
#   html_table()
# mid <-
#   mid[[1]] %>%
#   as_tibble() %>%
#   rename(country = Country) %>%
#   separate(col = "Codes", into = paste0("c", 1:20)) %>%
#   gather(dummy, mid, -country) %>%
#   drop_na() %>%
#   select(-dummy)
#
# mid <-
#   mid %>%
#   mutate(iso2 = countrycode(country, "country.name", "iso2c")) %>%
#   mutate(iso2 = case_when(mid == "303" ~ "US",
#                          mid == "608" ~ "GB",
#                          mid == "204" ~ "PT",
#                          mid == "306" ~ "NL",
#                          mid == "618" ~ "FR",
#                          mid == "635" ~ "FR",
#                          mid == "255" ~ "PT",
#                          mid == "661" ~ "RW",
#                          mid == "607" ~ "FR",
#                          TRUE ~ iso2))
# mid <-
#   mid %>%
#   select(MID = mid, ISO2 = iso2, COUNTRY = country)
# dbWriteTable(con, name = "VESSEL_MID", value = mid, overwrite = TRUE)

vid_mid <- function(con) {
  tbl_mar(con, "ops$einarhj.VESSEL_MID")
}




# ------------------------------------------------------------------------------
# Vessel call signs - ITU prefixes
# library(rvest)
# library(countrycode)
# library(tidyverse)
#url <- "https://en.wikipedia.org/wiki/ITU_prefix#Allocation_table"

# need to check Swaziland and Fiji
# x <-
#   rio::import("../ITU_prefix.csv", setclass = "tibble") %>%
#   janitor::clean_names() %>%
#   rename(cs = call_sign_series, cntr = allocated_to) %>%
#   mutate(cntr = str_replace(cntr, "\\[Note 1\\]", ""),
#          cntr = str_replace(cntr, "\\[Note 2\\]", ""),
#          cntr = str_replace(cntr, "\\[Note 4\\]", ""),
#          cntr = ifelse(str_starts(cntr, "France"), "France", cntr),
#          cntr = ifelse(str_starts(cntr, "United Kingdom"), "United Kingdom", cntr),
#          cntr = ifelse(str_starts(cntr, "Canada"), "Canada", cntr),
#          cntr = ifelse(str_starts(cntr, "Hong Kong"), "Hong Kong", cntr),
#          cntr = ifelse(str_starts(cntr, "Macao"), "Macao", cntr),
#          cntr = ifelse(str_starts(cntr, "Netherlands"), "Netherlands", cntr),
#          cntr = ifelse(str_detect(cntr, "Bosnia and Herzegovina"), "Bosnia and Herzegovina", cntr)) %>%
#   filter(!cntr %in% c("", "Republic of China (Taiwan)",
#                       "Liechtenstein (uses prefixes allocated to Switzerland)",
#                       "Swaziland", "Fiji")) %>%
#   add_row(cs = "BM-BQ", cntr = "Republic of China (Taiwan)") %>%
#   add_row(cs = "BU-BX", cntr = "Republic of China (Taiwan)") %>%
#   add_row(cs = c("HB0", "HB3Y", "HBL"), cntr = rep("Liechtenstein", 3)) %>%
#   separate(cs, c("from", "to", "to2"), remove = FALSE) %>%
#   mutate(to = ifelse(!is.na(to), to, from),
#          first = str_sub(from, 1, 1),
#          t1 = str_sub(from, 2, 2),
#          t2 = str_sub(to,   2, 2))
#
# # Poor mans loop
# n <- length(c(1:9, LETTERS))
# ltrs <- 1:n
# names(ltrs) <- c(1:9, LETTERS)
# # NOTE: Needs further work
# res <- list()
# for(i in 1:nrow(x)) {
#   print(i)
#   if(x$cs[[i]] %>% nchar() == 1) {
#     res[[i]] <- tibble(cs = x$cs[[i]], cntr = x$cntr[[i]])
#   } else {
#     res[[i]] <-
#       tibble(cs = paste0(x$first[[i]], names(ltrs[ltrs[[x$t1[[i]]]]:ltrs[[x$t2[[i]]]]])),
#              cntr = x$cntr[[i]])
#   }
# }
# ITU_prefix <-
#   bind_rows(res) %>%
#   mutate(iso2 = countrycode(cntr, "country.name", "iso2c"),
#          iso3 = countrycode(cntr, "country.name", "iso3c")) %>%
#   rename(CS_PREFIX = cs, COUNTRY = cntr, ISO2 = iso2)
#
# dbWriteTable(con, name = "VESSEL_CS_ITU_PREFIX", value = ITU_prefix, overwrite = TRUE)

vid_csprefix <- function(con) {
  tbl_mar(con, "ops$einarhj.VESSEL_CS_ITU_PREFIX")
}



# ------------------------------------------------------------------------------
# Vessel table -----------------------------------------------------------------
# NOTE: A static file, date notes time of last uploading

# Official registry

# library(tidyverse)
# library(mar)
# con <- connect_mar()
#
# # Offical regstry --------------------------------------------------------------
# kvoti.skipaskra_siglo <-
#   mar:::vessel_registry(con, TRUE) %>%
#   collect(n = Inf) %>%
#   filter(vid > 1) %>%
#   mutate(uid = str_replace(uid, "\\.", ""),
#          source = "registry",
#          iso2 = "IS")
# # Additional vessels kept by fiskistofa ----------------------------------------
# vlookup <- function(this, df, key, value) {
#   m <- match(this, df[[key]])
#   df[[value]][m]
# }
# orri.skipaskra <-
#   lesa_skipaskra(con) %>%
#   collect(n = Inf) %>%
#   filter(!skip_nr %in% kvoti.skipaskra_siglo$vid) %>%
#   mutate(name = str_trim(heiti),
#          einknr = ifelse(einknr %in% c(0, 999), NA_real_, einknr),
#          einknr = str_pad(einknr, width = 3, side = "left", pad = "0"),
#          einkst = ifelse(einkst %in% c("??", "X"), NA_character_, einkst),
#          uid = ifelse(!is.na(einknr), paste0(einkst, "-", einknr), einkst)) %>%
#   select(vid = skip_nr,
#          name = heiti,
#          uid,
#          brl,
#          length = lengd,
#          fclass = flokkur) %>%
#   mutate(length = ifelse(length <= 1, NA_real_, length),
#          brl = ifelse(brl == 0 |
#                         (brl < 1.0001 & is.na(length)) |
#                         (brl < 1.0001 & length > 15),
#                       NA_real_,
#                       brl),
#          source = "skipaskra") %>%
#   separate(name, c("dummy", "cs"), sep = "\\(", remove = FALSE) %>%
#   select(-dummy) %>%
#   mutate(cs = str_replace(cs, "\\)", ""),
#          cs = str_trim(cs),
#          cs = ifelse(vid >= 5000, "TF", cs),
#          cs_prefix = str_sub(cs, 1, 2)) %>%
#   left_join(mar:::vessel_csprefix(con) %>%
#               select(cs_prefix, iso2) %>%
#               collect(n = Inf)) %>%
#   mutate(iso2 = case_when(!is.na(iso2) ~ str_sub(uid, 1, 2),
#                           TRUE ~ iso2)) %>%
#   select(-c(cs_prefix))
# # orri.skipaskra %>%
# #   filter(!is.na(cs)) %>%
# #   group_by(cs) %>%
# #   mutate(n.cs = n()) %>%
# #   ungroup() %>%
# #   arrange(-n.cs, cs) %>%
# #   select(cs, n.cs, vid, name, uid) %>%
# #   left_join(tbl_mar(con, "kvoti.skipasaga") %>% select(vid = skip_nr, saga_nr:ur_gildi, heiti) %>%
# #               collect(n = Inf))
# ## Some vessels pickup up from the MMSI registry -------------------------------
# einarhj.VESSEL_MMSI_20190627 <-
#   mar:::vessel_mmsi(con) %>%
#   filter(!is.na(vid)) %>%
#   collect(n = Inf) %>%
#   filter(!vid %in% c(kvoti.skipaskra_siglo$vid, orri.skipaskra$vid)) %>%
#   select(vid, name, cs) %>%
#   separate(name, c("name", "uid"), sep = " ") %>%
#   mutate(uid = str_sub(uid, 1, 2),
#          source = "mmsi",
#          iso2 = "IS")
# ## Join the stuff --------------------------------------------------------------
# vessels <-
#   bind_rows(kvoti.skipaskra_siglo,
#             orri.skipaskra,
#             einarhj.VESSEL_MMSI_20190627) %>%
#   arrange(vid) %>%
#   mutate(cntr = ifelse(nchar(uid) == 2, uid, NA_character_)) # %>%
#   # NOTE: Below gives the wrong match on cs
#   # 3899	Kaldbakur flutningaskip (TFBC)	TFBC	NA	IS	101227	1395	TFBC	261044	1
#   # mutate(cs = ifelse(vid == 3899, NA_character_, cs))
# vessels %>% write_rds("data/vessels.rds")
# VS <- vessels %>% select_all(toupper)
# dbWriteTable(con, name = "VESSELS", value = vessels, overwrite = TRUE)

vid_vessels <- function(con) {
  tbl_mar(con, "ops$einarhj.VESSELS")
}

# Vessel class - get the proper one from siglo ---------------------------------
# vclass <-
#   tribble(~code, ~flokkur, ~class,
#           0L,      "unspecified", "unspecified",
#           33L,     "FISKISKIP",   "fishing",
#           35L,     "SKUTTOGARI", "fishing",
#           36L,     "NÓTAVEIÐI, SKUTTOGARI", "fishing",
#           37L,     "HVALVEIÐISKIP",    "whaler",
#           38L,     "unspecified", "unspecified",
#           39L,     "vöruflutningaskip", "cargo",
#           40L,     "unspecified", "unspecified",
#           41L,     "FARÞEGASKIP", "passenger",
#           42L,     "VARÐSKIP", "coast guard",
#           43L,     "SKÓLASKIP", "school ship",
#           44L,     "RANNSÓKNARSKIP", "research",
#           45L,     "SJÓMÆLINGASKIP", "research",
#           46L,     "BJÖRGUNARSKIP", "sar",
#           48L,     "OLÍUFLUTNINGASKIP", "tanker",
#           49L,     "olíuskip", "tanker",
#           50L,     "DRÁTTARSKIP", "tug boat",
#           51L,     "unspecified", "unspecified",
#           53L,     "LÓÐSSKIP", "pilot vessel",
#           54L,     "VINNUSKIP", "utility vessel",
#           55L,     "DÝPK. OG SANDSKIP", "hopper dredger",
#           56L,     "DÝPKUNARSKIP", "dredger",
#           57L,     "PRAMMI", "barge",
#           58L,     "FLOTBRYGGJA", "flotbryggja",
#           59L,     "FLOTKVÍ", "flotkví",
#           60L,     "SEGLSKIP", "sailing vessel",
#           61L,     "VÍKINGASKIP", "longboat",
#           62L,     "SKEMMTISKIP", "passenger?",
#           63L,     "AFSKRÁÐUR", "Decomissioned",
#           64L,     "FISKI, FARÞEGASKIP", "turist fisher",
#           65L,     "HAFNSÖGU, DRÁTTARSKIP", "pilot/tugboat",
#           66L,     "ÞANGSKURÐARPRAMMI", "kelp vessel",
#           67L,     "unspecified", "unspecified",
#           68L,     "FRÍSTUNDAFISKISKIP", "pleasure vessel",
#           69L,     "EFTIRLITS- OG BJÖRGUNARSKIP", "unspecified",
#           70L,     "unspecified", "unspecified",
#           73L,     "FARÞEGABÁTUR", "passenger",
#           74L,     "FISKI, FARÞEGABÁTUR", "turist fisher",
#           75L,     "SJÓKVÍA VINNUSKIP", "utility vessel",
#           NA_integer_, NA_character_, NA_character_) %>%
#   select_all(toupper)
# vclass %>% count(CODE) %>% filter(n > 1)
# vclass %>% count(FLOKKUR) %>% filter(n > 1)
# vclass %>% count(CLASS) %>% filter(n > 1)
# dbWriteTable(con, name = "VESSEL_CLASS", value = vclass, overwrite = TRUE)

vid_class <- function(con) {
  tbl_mar(con, "ops$einarhj.VESSEL_CLASS")
}


## Siglingamálastofnun - skipaflokkur




