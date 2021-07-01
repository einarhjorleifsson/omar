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