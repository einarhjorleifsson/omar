#' ISSCFG codes
#'
#' @description International Standard Statistical Classification of Fishing Gear (ISSCFG), 
#' see http://www.fao.org/fishery/docs/DOCUMENT/cwp/handbook/annex/AnnexM2fishinggear.pdf and
#' https://www.fao.org/documents/card/en/c/cb4966en
#' 
#' @param con oracle connection
#'
#' @return A query
#' @export
#' 
gid_isscfg <- function(con) {
  tbl_mar(con, "ops$einarhj.gid_isscfg")
}

#' ISSCFG category table
#'
#' @description International Standard Statistical Classification of Fishing Gear (ISSCFG), 
#' see http://www.fao.org/fishery/docs/DOCUMENT/cwp/handbook/annex/AnnexM2fishinggear.pdf and
#' https://www.fao.org/documents/card/en/c/cb4966en
#' 
#' @param con oracle connection
#'
#' @return A query
#' @export
#' 
gid_isscfg_category <- function(con) {
  tbl_mar(con, "ops$einarhj.gid_isscfg_category")
}


#' Orri gear with isscfg
#'
#' @param con oracle connection
#'
#' @return A query
#' @export
#'
gid_orri_plus <- function(con) {
  tbl_mar(con, "ops$einarhj.gid_orri_plus") %>% 
    dplyr::select(gid = veidarfaeri,
                  gid2,
                  veidarfaeri = lysing,
                  gear = lysing_enska,
                  gid_fi = fi_veidarfaeri,
                  gid_lods = lods_veidarfaeri,
                  isscfg, 
                  everything())
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

