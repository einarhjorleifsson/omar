#' bilen
#'
#' Reads biota.lengd_skalad_v that includes a raising factor and 
#' provides weight as a function of length. No raising done. If species only 
#' counted at station, not included in algorithm.
#' 
#' @param con oracle connection
#' @param std create standardized/shortcut names (default is TRUE)
#' @param trim trim return only key variables (default is TRUE). Only operational
#' if std is TRUE.
#' @param weights Gets or calculates weights (default is TRUE). Only operational
#' if std is TRUE.
#'
#' @return a tibble query with the following variables if arguement std is TRUE (default):
#' \describe{
#'   \item{.id}{synis_id)}
#'   \item{sid}{species id}
#'   \item{length}{length, normally in centimeters)}
#'   \item{wt}{derived weights in grams, estimated from length weight relationship. If parameters not available in biota.lw_coeffs then 0.01 and 3 are used}
#'   \item{sex}{sex, normally 1 is male, 2 is female}
#'   \item{mat}{maturity stage}
#'   \item{age}{age, normally in years}
#'   \item{n}{number of measured fish}
#'   \item{rn}{the count raising factor}
#'   \item{mwt}{the estimated weights in grams for a given length}
#' }
#' 
#' @export
#'
bi_len <- function(con, std = TRUE, trim = TRUE, weights = TRUE) {
  q <-
    tbl_mar(con, "biota.lengd_skalad_v") |> 
    dplyr::mutate(r_talid = nvl(r_talid, 1))
  
  if(std) {
    q <- 
      q |> 
      dplyr::rename(dplyr::any_of(vocabulary))
      
    
    if(trim) {
      q <-
        q %>% 
        dplyr::select(.id:rn)
    }
    if(weights) {
      q <- 
        q |> 
        dplyr::left_join(bi_lwcoeffs(con),
                         by = "sid") |> 
        dplyr::mutate(a = ifelse(is.na(a), 0.01, a), 
                      b = ifelse(is.na(b), 3, b), 
                      mwt = a * length^b) %>% 
        dplyr::select(-c(a, b))
    }
    
  }
  
  return(q)
}

#' Length weight coefficent of some selected species
#'
#' @param con oracle connection
#'
#' @return A query with sid (species number), a and b
#' @export
#'
bi_lwcoeffs <- function(con) {
  tbl_mar(con, "biota.lw_coeffs") |> 
    dplyr::rename(sid = species)
}

#' by_age
#'
#' Reads biota.aldur_v
#' 
#' @param con oracle connection
#' @param std create standardized/shortcut names (default is TRUE)
#'
#' @return a tibble query
#' @export
#'
bi_age <- function(con, std = TRUE) {
  
  q <- 
    tbl_mar(con, "biota.aldur_v") |> 
    dplyr::select(maeling_id,
                  synis_id,
                  tegund_nr,
                  lengd,
                  kyn_nr,
                  kynthroski_nr,
                  aldur, 
                  fjoldi,
                  kvarna_nr,
                  thyngd,
                  slaegt,
                  magi,
                  lifur,
                  kynfaeri,
                  hlutfall,
                  dplyr::everything())
  
  if(std) {
    q <- 
      q %>% 
      dplyr::rename(dplyr::any_of(vocabulary))
      
  }
  
  return(q)
}


bi_scl <- function(con) {
  tbl_mar(con, "biota.skalar_v") %>% 
    dplyr::select(.id = synis_id,
                  sid = tegund_nr,
                  ot = kvarnadir,
                  le = maeldir,
                  n = taldir)
}




#' Biological measuremnts
#'
#' @param con oracle connection
#' @param std create standardized/shortcut names (default is TRUE)
#' @param trim trim return only key variables (default is TRUE)
#'
#' @return a query
#' @export
#'
bi_measure <- function(con, std = TRUE, trim = TRUE) {
  q <- 
    tbl_mar(con, "biota.measure") |> 
    dplyr::select(sample_id:note,
                  dplyr::everything())
  if(std) {
    q <- 
      q |> 
      dplyr::rename(.id = sample_id,
                    sid = species_no,
                    mtype = measure_type,
                    n = count,
                    sex = sex_no,
                    maturity = maturity_id,
                    wt = weight,
                    gwt = gutted,
                    gonads = genital)
    if(trim) {
      q <-
        q |> 
        dplyr::select(.id:measure_id)
    }
  } else {
    if(trim) {
      q <-
        q |> 
        dplyr::select(sample_id:measure_id)
    }
  }
  return(q)
}
  
