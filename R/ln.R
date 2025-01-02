#' Landings (gafl)
#'
#' @param con oracle connection
#' @param trim only return essential variables (default)
#'
#' @return a query
#' @export
#'
ln_agf <- function(con, trim = TRUE) {
  q <-
    tbl_mar(con, "agf.aflagrunnur") |> 
    dplyr::select(.id = londun_id,
                  date = londun_hefst,
                  hid = hafnarnumer,
                  vid = skip_numer,
                  gid = veidarfaeri,
                  stock = veidistofn,
                  sid = fisktegund,
                  astand,
                  wt = magn_oslaegt,
                  gt = magn_slaegt,
                  magn,
                  dplyr::everything())
  if(trim) {
    q <-
      q |> 
      dplyr::select(.id:magn)
  }
  return(q)
}


#' landings (kvoti.lods_londun)
#'
#' @param con oracle connection
#' @param standardize Standardize (default TRUE) renames some field and then 
#' some but returns all variables
#' 
#' @export
#'
ln_landing <- function(con, standardize = TRUE) {
  
  q <- 
    omar::tbl_mar(con, "kvoti.lods_londun")
  
  if(standardize) {
    q <-
      q %>% 
      dplyr::select(vid = skip_nr,
                    hid = hofn,
                    ID = komunr,
                    date = l_dags,
                    gid = veidarf,
                    selj,
                    stada,
                    linu_ivilnun) %>% 
      dplyr::mutate(year = year(date))
  }
  
  return(q)
  
}


#' landed catch (kvoti.lods_oslaegt)
#'
#' @param con oracle connection
#' @param standardize Standardize (default TRUE) renames some field and then 
#' some but returns all variables
#' 
#' @export
#'
ln_catch <- function(con, standardize = TRUE) {
  
  q <- 
    omar::tbl_mar(con, "kvoti.lods_oslaegt")
  
  if(standardize) {
    q <-
      q %>% 
      dplyr::rename(vid = skip_nr,
                    hid = hofn,
                    ID = komunr,
                    date = l_dags,
                    sid = fteg,
                    catch = magn_oslaegt,
                    area = veidisvaedi,
                    gid = veidarf) %>% 
      dplyr::mutate(year = year(date))
  }
  
  return(q)
  
}

#' landed catch (fiskifelagid.vigtarskra66_81)
#'
#' @param con oracle connection
#' @param standardize Standardize (default TRUE) renames some field and then
#' some but returns all variables
#'
#' @return a query
#' @export
#'
ln_1966_1981 <- function(con, standardize = TRUE) {
  
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


#' landed catch (fiskifelagid.landed_catch_pre94)
#'
#' @param con oracle connection
#' @param standardize Standardize (default TRUE) renames some field and then
#' some but returns all variables
#'
#' @return a query
#' @export
#'
ln_1982_1993 <- function(con, standardize = TRUE) {
  
  q <- 
    tbl_mar(con, "fiskifelagid.landed_catch_pre94") 
  
  if(standardize) {
    
    q <- 
      q %>% 
      dplyr::rename(gid = veidarfaeri,
                    vid = skip_nr,
                    sid = fteg,
                    year = ar,
                    month = man,
                    hid = hofn,
                    catch = magn_oslaegt,
                    area = veidisvaedi) 
  }
  
  return(q)
  
}
