# ICELANDIC MMSI ---------------------------------------------------------------
# NOTE: The sknr is both the conventional numerical "skipaskrarnumer" and
#       vessels that have "SKB" as a prefix
# pth <- "https://www.fjarskiptastofa.is/library?itemid=6e223f8d-0a33-45ae-8fb8-a3997ec66cc5"
# download.file(pth, destfile = "tmp.xlsx")
# v_mmsi <-
#   readxl::read_excel("tmp.xlsx") %>%
#   janitor::clean_names() %>%
#   dplyr::select_all(toupper) %>%
#   arrange(SKNR) %>% 
#   rename(VESSEL = SKIP,
#          CS = KALLM,
#          MMSI = MMSI_NR,
#          STDC = STANDARD_C) %>% 
#   dplyr::mutate(VID = case_when(str_sub(MMSI, 1, 3) == "251" ~ as.integer(SKNR),
#                                 TRUE ~ NA_integer_),
#                 VID2 = case_when(str_sub(MMSI, 1, 3) != "251" ~ as.integer(SKNR),
#                                  TRUE ~ NA_integer_),
#                 VESSEL = str_replace(VESSEL, " -", "-"),
#                 VESSEL = str_replace(VESSEL, "KO-", "KÓ-"))
# pos <- str_locate(v_mmsi$VESSEL, "-")[, 1] - 2
# UST <- omar::vessel_ust(con) %>% collect() 
# v_mmsi <- 
#   v_mmsi %>% 
#   mutate(UID = case_when(is.na(VID) ~ NA_character_,
#                          VESSEL == "Birta VH-39" ~ "VE-39",
#                          VESSEL == "Dala-Rafn VE-508" ~ "VE-508",
#                          VESSEL == "Villi-Björn SH-148" ~ "SH-148",
#                          TRUE ~ str_sub(VESSEL, pos))) %>% 
#   mutate(VESSEL = ifelse(is.na(UID), VESSEL, str_sub(VESSEL, 1, nchar(VESSEL) - nchar(UID) - 1)),
#          VESSEL = str_to_upper(str_trim(VESSEL)))
# v_mmsi.vessel <- 
#   v_mmsi %>% 
#   filter(str_sub(MMSI, 1, 3) == "251")
# v_mmsi.aux <- 
#   v_mmsi %>% 
#   filter(str_sub(MMSI, 1, 3) != "251") %>% 
#   mutate(VESSEL = str_replace(VESSEL, "- FYLGIBÁTUR", ""),
#          VESSEL = str_replace(VESSEL, "-FYLGIBÁTUR", ""),
#          VESSEL = str_replace(VESSEL, "- FYLGIBÁTUT", ""),
#          VESSEL = str_replace(VESSEL, "- FYLIGBÁTUR", ""),
#          VESSEL = str_replace(VESSEL, "- FYLGIBATUR", ""),
#          VESSEL = str_trim(VESSEL),
#          ATHUGASEMDIR = ifelse(str_detect(str_to_upper(ATHUGASEMDIR), "FYLGIBÁTUR"),
#                                "Fylgibátur", ATHUGASEMDIR),
#          ATHUGASEMDIR = ifelse(str_detect(ATHUGASEMDIR, "Selt"), "Fylgibátur", ATHUGASEMDIR),
#          ATHUGASEMDIR = str_replace(ATHUGASEMDIR, "nr.", ""),
#          ATHUGASEMDIR = str_squish(ATHUGASEMDIR),
#          ATHUGASEMDIR = str_replace(ATHUGASEMDIR, "ASI", "AIS"),
#          ATHUGASEMDIR = str_replace(ATHUGASEMDIR, "-netbauja", " netbauja"),
#          ATHUGASEMDIR = str_replace(ATHUGASEMDIR, "netbayja", "netabauja"),
#          ATHUGASEMDIR = str_replace(ATHUGASEMDIR, "netabuja", "netabauja"),
#          ATHUGASEMDIR = str_replace(ATHUGASEMDIR, "netabauja", "netabauja")) %>% 
#   left_join(v_mmsi.vessel %>% select(SKNR, VESSEL2 = VESSEL, UID2 = UID)) %>% 
#   mutate(VESSEL = VESSEL2,
#          UID = UID2) %>% 
#   select(-c(VESSEL2, UID2))
# v_mmsi.final <-
#   bind_rows(v_mmsi.vessel, v_mmsi.aux) %>% 
#   arrange(SKNR)
# DBI::dbWriteTable(con, name = "VESSEL_MMSI_20220405", value = v_mmsi.final, overwrite = TRUE)

#' Vessel MMSI
#' 
#' Table containing vessel MMSI, including auxillary numbers such as 
#' navigational aid (like net buoy) 
#' and daughter craft. Last version obtained 2022-04-05
#'
#' @param con sql connection
#'
#' @return sql query
#' @export
#' @source \url{https://www.fjarskiptastofa.is}
mmsi_vessel <- function(con) {
  tbl_mar(con, "ops$einarhj.VESSEL_MMSI_20220405")
}

mmsi_vessel_old <- function(con) {
  tbl_mar(con, "ops$einarhj.VESSEL_MMSI_20190627")
}



#' Maritime identification digits - country code
#' 
#' @param con Oracle connection
#'
#' @return An sql query
#' @export
#'
#' @source \url{https://en.wikipedia.org/wiki/Maritime_identification_digits}
mmsi_mid <- function(con) {
  tbl_mar(con, "ops$einarhj.MMSI_MID")
}

#' Establish the MMSI type
#'
#' @param mmsi A vector
#'
#' @return A vector
#' @export
#'
mmsi_class <- function(mmsi) {
  dplyr::case_when(stringr::str_sub(mmsi, 1, 1) == "1" ~ "SAR",
                   stringr::str_sub(mmsi, 1, 1) %in% as.character(2:7) ~ "vessel",
                   stringr::str_sub(mmsi, 1, 1) == "8" ~ "vhf",
                   stringr::str_sub(mmsi, 1, 3) == "970" ~ "search_and_rescue",
                   stringr::str_sub(mmsi, 1, 3) == "972" ~ "man overboard",
                   stringr::str_sub(mmsi, 1, 3) == "974" ~ "epirb",
                   stringr::str_sub(mmsi, 1, 2) == "98" ~ "daughter craft",
                   stringr::str_sub(mmsi, 1, 2) == "99" ~ "navigational aid",
                   TRUE ~ NA_character_)
}

mmsi_extract_mid <- function(mmsi) {
  tibble::tibble(mmsi = mmsi) %>% 
    dplyr::mutate(mmsi_class = mmsi_class(mmsi)) %>% 
    dplyr::mutate(MID = dplyr::case_when(mmsi_class == "vessel" ~ stringr::str_sub(mmsi, 1, 3),
                                         mmsi_class == "SAR" & stringr::str_sub(mmsi, 1, 3) == "111" ~ stringr::str_sub(mmsi, 4, 6),
                                         mmsi_class %in% c("daughter craft", "navigational aid") ~ stringr::str_sub(mmsi, 3, 5),
                                         TRUE ~ NA_character_)) %>% 
    dplyr::pull(MID)
}
