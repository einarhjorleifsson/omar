#' biota tegundir
#'
#' @param con oracle connection
#' @param std standardize some variable names (default TRUE)
#' @param trim return only selected variable names (default TRUE)
#'
#' @return a query
#' 
#' @export
sid_biota <- function(con, std = TRUE, trim = TRUE) {
  
  if(std) {
    if(trim) {
      q <-
        tbl_mar(con, "biota.tegund_v") %>% 
        dplyr::select(sid = tegund_nr,
                      tegund = heiti,
                      latin = visinda_heiti,
                      species = enskt_heiti)
    } else {
      q <- 
        tbl_mar(con, "biota.tegund_v") %>% 
        dplyr::select(sid = tegund_nr,
                      tegund = heiti,
                      latin = visinda_heiti,
                      species = enskt_heiti,
                      dplyr::everything())
    }
  } else {
    q <- tbl_mar(con, "biota.tegund_v")
  }
  
  return(q)
  
}


#' biota length-weight coefficients
#'
#' @param con oracle connection
#'
#' @return a query
#' 
#' @export
sid_lwcoeffs <- function(con) {
  tbl_mar(con, "biota.lw_coeffs") %>% 
    dplyr::rename(sid = species)
}
