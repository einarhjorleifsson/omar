bi_len <- function(con, std = TRUE) {
  
  q <- tbl_mar(con, "biota.lengd_v")
  
  if(std) {
    q <-
      q %>% 
      dplyr::select(mid = maeling_id,
                    id = synis_id,
                    sid = tegund_nr,
                    length = lengd,
                    sex = kyn_nr,
                    mat = kynthroski_nr,
                    age = aldur,
                    n = fjoldi,
                    rate = hlutfall)
  }
  
  return(q)
  
}

bi_age <- function(con, std = TRUE) {
  
  q <- tbl_mar(con, "biota.aldur_v")
  
  if(std) {
    q <- 
      q %>% 
      dplyr::select(mid = maeling_id,
                    id = synis_id,
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
                    gonads = kynfaeri,
                    rate = hlutfall)
  }
  
  return(q)
}

bi_mea <- function(con) {
  q <- 
    tbl_mar(con, "biota.measure")
}
