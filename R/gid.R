#' Title
#'
#' @description International Standard Statistical Classification of Fishing Gear (ISSCFG), see http://www.fao.org/fishery/docs/DOCUMENT/cwp/handbook/annex/AnnexM2fishinggear.pdf
#' 
#' @param con oracle connection
#'
#' @return A query
#' 
std_isscfg <- function(con) {
  tbl_mar(con, "ops$einarhj.isscfg")
}

#' Orri gear with isscfg
#'
#' @param con oracle connection
#'
#' @return A query
#'
gid_isscfg <- function(con) {
  tbl_mar(con, "ops$einarhj.gear_isscfg")
}

#' orri.veidarfaeri
#'
#' @param con oracle connection
#'
#' @return a query
#' @export
#'
gid_orri <- function(con) {
  tbl_mar(con, "orri.veidarfaeri") %>% 
    dplyr::select(gid = veidarfaeri,
                  veidarfaeri = lysing,
                  gear = lysing_enska,
                  gid_fi = fi_veidarfaeri,
                  gid_lods = lods_veidarfaeri)
}

#' Gear correction table
#'
#' @param con oracle connection
#'
#' @return a query
#' @export
#'
gid_correction <- function(con) {
  tbl_mar(con, "ops$einarhj.LB_GEAR_CORRECTION") %>% 
    dplyr::rename(gidc = gid)
}

