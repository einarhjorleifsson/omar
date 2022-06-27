#' bi_len
#'
#' Reads biota.lengd_skalad_v that includes a raising factor and 
#' provides weight as a function of length. No raising done.
#' 
#' NOTE: If species only counted at station, not included in algorithm.
#' 
#' @param con oracle connection
#' @param std create standardized/shortcut names (default is TRUE)
#' @param trim trim return only key variables (default is TRUE). only operational
#' if std is TRUE
#'
#' @return a tibble query
#' @export
#'
bi_len <- function(con, std = TRUE, trim = TRUE) {
  q <-
    tbl_mar(con, "biota.lengd_skalad_v") 
  
  if(std) {
    q <- 
      q %>% 
      dplyr::rename(.id = synis_id,
                    sid = tegund_nr,
                    length = lengd,
                    sex = kyn_nr,
                    mat = kynthroski_nr,
                    age = aldur,
                    n = fjoldi,
                    rn = r_talid) %>% 
      dplyr::mutate(rn = nvl(rn, 1)) %>% 
      dplyr::left_join(sid_lwcoeffs(con),
                       by = "sid") %>% 
      dplyr::mutate(a  = ifelse(is.na(a), 0.01, a),
                    b  = ifelse(is.na(b), 3.00, b),
                    wt = (a * length^b) / 1e3, .after = length) %>% 
      dplyr::select(-c(a, b))
    if(trim) {
      q <-
        q %>% 
        dplyr::select(.id:rn)
    }
  }
  
  return(q)
}

#' by_age
#'
#' Reads biota.aldur_v
#' 
#' @param con oracle connection
#' @param std create standardized/shortcut names (default is TRUE)
#' @param trim trim return only key variables (default is TRUE). only operational
#' if std is TRUE
#'
#' @return a tibble query
#' @export
#'
bi_age <- function(con, std = TRUE, trim = TRUE) {
  
  q <- tbl_mar(con, "biota.aldur_v")
  
  if(std) {
    q <- 
      q %>% 
      dplyr::select(.mid = maeling_id,
                    .id = synis_id,
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
