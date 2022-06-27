#' Orri tegundir
#'
#' @param con oracle connection
#'
#' @return a query
#' 
#' @export
sid_orri <- function(con) {
  tbl_mar(con, "orri.fisktegundir") %>% 
    dplyr::select(sid = tegund, 
                  tegund = heiti,
                  species = enskt_heiti,
                  latin = visindaheiti,
                  yfir_flokkur)
}

#' biota tegundir
#'
#' @param con oracle connection
#'
#' @return a query
#' 
sid_biota <- function(con) {
  tbl_mar(con, "biota.tegund_v") %>% 
    dplyr::rename(sid = tegund_nr,
                  heiti = tegund,
                  latin = visinda_heiti,
                  species = enskt_heiti)
}


sid_lwcoeffs <- function(con) {
  tbl_mar(con, "biota.lw_coeffs") %>% 
    dplyr::rename(sid = species)
}
