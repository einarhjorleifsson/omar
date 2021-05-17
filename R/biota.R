qlen <- function(con) {
  mar::les_lengd(con) %>% 
    dplyr::rename(id = synis_id,
                  sid = tegund_nr,
                  length = lengd,
                  sex = kyn_nr,
                  mat = kynthroski_nr,
                  age = aldur, 
                  n = fjoldi)
}
qage <- function(con) {
  mar::les_aldur(con) %>% 
    dplyr::rename(id = synis_id,
                  sid = tegund_nr,
                  length = lengd,
                  sex = kyn_nr,
                  mat = kynthroski_nr,
                  age = aldur, 
                  n = fjoldi,
                  knr = kvarna_nr,
                  wt = thyngd,
                  gwt = slaegt,
                  stomach = magi,
                  liver = lifur,
                  gonads = kynfaeri)
}
qmes <- function(con) {
  mar::tbl_mar(con, "biota.measure")
}
