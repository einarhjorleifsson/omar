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
                  dplyr::everything())
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

# Ideally this function should both work live as well as a lookup on archieves
# Here we have thess possibilites:
#  1. Run a crontab, updating the lookup table, like once a month (week?)
#  2. Have argument in the function to either use the lookup or not
#     Could one use the lookup on the historical and then "live" on the
#     the remainder?
#
# The code for updating the table is found in data-raw/00-SETUP_gear-correction.R
#' Gear correction table
#'
#' @param con oracle connection
#' 
#' @return a query containing: visir, id and gid where the latter is the corrected
#' gear identification
#' @export
#'
gid_correction <- function(con) {
  tbl_mar(con, "ops$einarhj.LB_GEAR_CORRECTION") %>% 
    dplyr::rename(gidc = gid)
}

