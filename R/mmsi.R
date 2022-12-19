#' MMSI Icelandic registry
#' 
#' Table containing among other things vessel MMSI, including auxillary numbers such as 
#' navigational aid (like net buoy) 
#' and daughter craft. Last version obtained 2022-12-02
#'
#' @param con sql connection
#'
#' @return sql query
#' @export
#' @source \url{https://www.fjarskiptastofa.is/english/telecom-affairs/maritime-communications}
#' 
mmsi_icelandic_registry <- function(con) {
  tbl_mar(con, "ops$einarhj.MMSI_ICELANDIC_REGISTRY")
}

mmsi_vessel_old2 <- function(con) {
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
