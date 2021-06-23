#' Title
#'
#' @param con oracle connection
#' @param standardize Standardize (default TRUE) renames some field and then some but returns all variables
#'
#' @return a query
#' @export
#'
ln_vigtarskra <- function(con, standardize = TRUE) {
  q <- 
    tbl_mar(con,'fiskifelagid.vigtarskra66_81')
  
  if(standardize) {
    q <- 
      q %>% 
      dplyr::mutate(catch = magn * reiknistudull) %>% 
      dplyr::select(year = artal,
             month = manudur,
             das = uthaldsdagar,
             vid = skip_nr,
             hid = vinnsluhofn,
             gid = veidarfaeri,
             sid = fteg,
             catch,
             dplyr::everything())
  }
  
  return(q)
  
}
