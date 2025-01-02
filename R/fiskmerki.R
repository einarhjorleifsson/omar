#' @title merki
#'
#' @name fiskmerki_merki
#'
#' @description XXX
#'
#' @param con Connection to mar Oracle database
#'
#' @export

fiskmerki_merki <- function(con) {
  d <- tbl_mar(con, "fiskmerki.merki") %>%
    dplyr::rename(tid = id)
  return(d)
}

#' @title fiskar
#'
#' @name fiskmerki_fiskar
#'
#' @description XXX
#'
#' @param con src_oracle tenging við oracle
#'
#' @export
fiskmerki_fiskar <- function(con) {
  
  d <- tbl_mar(con, "fiskmerki.fiskar") %>%
    dplyr::rename(fiskur_id = id,
                  tsid = tegund,
                  tlen = lengd,
                  twgt = thyngd,
                  tsex = kyn,
                  tmat = kynthroski)
  
  return(d)
}

#' @title endurheimtur
#'
#' @description XXX
#'
#' @name fiskmerki_endurheimtur
#'
#' @param con src_oracle tenging við oracle
#'
#' @export
fiskmerki_endurheimtur <- function(con) {
  d <- 
    tbl_mar(con, "fiskmerki.endurheimtur") %>% 
    dplyr::select(rid = id,
                  tid = merki_id,
                  rdate = dags_fundid,
                  ryear = ar,
                  rmonth = manudur,
                  rlon = vlengd,
                  rlat = nbreidd,
                  rr = reitur,
                  rsr = smareitur,
                  rarea = landshluti_id,
                  rocean = hafsvaedi_id,
                  rgid = veidarfaeri_id,
                  rz = dypi,
                  rvid = skip_nr,
                  rvessel = skip_nafn,
                  rsid = tegund,
                  rlen = lengd,
                  rsex = kyn,
                  rmat = kynthroski,
                  rage = aldur) %>%
    dplyr::mutate(rlon = -geoconvert1(rlon * 100),
                  rlat =  geoconvert1(rlat * 100))
  
  return(d)
}

#' @title rafaudkenni
#'
#' @description XXX
#'
#' @name fiskmerki_rafaudkenni
#'
#' @param con src_oracle tenging við oracle
#'
#' @export
fiskmerki_rafaudkenni <- function(con) {
  
  d <- tbl_mar(con, "fiskmerki.rafaudkenni") %>%
    dplyr::rename(tid = merki_id,
                  dst_id = taudkenni)
  return(d)
}

#' @title dst
#'
#' @name fiskmerki_rafgogn
#'
#' @description XXX
#'
#' @param con src_oracle tenging við oracle
#' @export
fiskmerki_rafgogn <- function(con) {
  
  d <- 
    tbl_mar(con, "fiskmerki.rafgogn") %>%
    dplyr::rename(dst_id = taudkenni,
                  time = dagstimi,
                  z = dypi,
                  temp = hitastig)
  
  return(d)
}


#' @title taggart
#'
#' @name taggart
#'
#' @description A flat dataframe of tags and recaptures
#'
#' @export
#'
#' @param con src_oracle tenging við oracle
taggart <- function(con) {
  
  stodvar <-
    mar::les_stod(con) %>%
    dplyr::left_join(mar::les_syni(con), by = 'stod_id') %>% 
    # dplyr::mutate(tLon = kastad_lengd,
    #        tLat =  kastad_breidd,
    #        tAr = to_number(to_char(dags, 'yyyy'))) %>%
    dplyr::select(synis_id,
                  cruise = leidangur,
                  station = stod_id,
                  tdate = dags,
                  tyear = ar,
                  tr = reitur,
                  tsr = smareitur,
                  tlon = kastad_lengd,
                  tlat = kastad_breidd,
                  tz = botndypi_kastad,
                  tgid = veidarfaeri)
  
  fiskar <-
    fiskmerki_fiskar(con) %>%
    dplyr::select(fiskur_id,
                  synis_id,
                  tsid,
                  tlen,
                  twgt,
                  tsex,
                  tmat)
  
  merki <-
    fiskmerki_merki(con) %>%
    dplyr::select(tid,
                  fiskur_id,
                  audkenni,
                  numer,
                  utgafa)
  
  d <-
    fiskar %>%
    dplyr::left_join(merki, by = "fiskur_id") %>%
    dplyr::left_join(stodvar, by = "synis_id") %>%
    dplyr::left_join(fiskmerki_endurheimtur(con), by = "tid") %>%
    dplyr::left_join(fiskmerki_rafaudkenni(con),  by = "tid") %>%
    dplyr::select(-id, -tid)
  
  return(d)
}

