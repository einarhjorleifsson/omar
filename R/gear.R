#' Title
#'
#' @description International Standard Statistical Classification of Fishing Gear (ISSCFG), see http://www.fao.org/fishery/docs/DOCUMENT/cwp/handbook/annex/AnnexM2fishinggear.pdf
#' 
#' @param con oracle connection
#'
#' @return A query
#' 
gear_isscfg <- function(con) {
  tbl_mar(con, "ops$einarhj.isscfg")
}

#' Orri gear with isscfg
#'
#' @param con 
#'
#' @return
#' @export
#'
gear_isscfg <- function(con) {
  tbl_mar(con, "ops$einarhj.gear_isscfg")
}

#' orri.veidarfaeri
#'
#' @param con 
#'
#' @return
#' @export
#'
lesa_veidarfaeri <- function(con) {
  tbl_mar(con, "orri.veidarfaeri") %>% 
    dplyr::select(gid = veidarfaeri,
           veidarfaeri = lysing,
           gear = lysing_enska,
           gid_fi = fi_veidarfaeri,
           gid_lods = lods_veidarfaeri)
}



