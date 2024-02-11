#' ch_station
#'
#' Reads channel.stod_v. Access the same view as mar::les_stod
#' 
#' @param con oracle connection
#' @param std boolean (default TRUE), create standardized/shortcut names
#' @param trim boolean (default TRUE), return only key variables. only operational
#' if std is TRUE
#'
#' @return a tibble query
#' @export
#'
ch_station <- function(con, std = TRUE, trim = TRUE) {
  
  q <- 
    tbl_mar(con, "channel.stod_v") |> 
    dplyr::select(stod_id,
                  skip_nr,
                  dags,
                  ar,
                  reitur,
                  smareitur,
                  kastad_lengd,
                  kastad_breidd,
                  hift_lengd,
                  hift_breidd,
                  botndypi_kastad,
                  botndypi_hift,
                  yfirbordshiti,
                  botnhiti,
                  dplyr::everything())
  
  if(std) {
    q <- 
      q |> 
      dplyr::rename(dplyr::any_of(vocabulary)) 
    
    if(trim) {
      q <- q %>% dplyr::select(.stid:bt)
    }
  }
  
  return(q)
  
}

#' ch_sample
#'
#' Reads channel.syni_v
#' 
#' @param con oracle connection
#' @param std create standardized/shortcut names (default is TRUE)
#' @param trim trim return only key variables (default is TRUE). only operational
#' if std is TRUE
#'
#' @return a tibble query, note towlength is in meters is std is set to TRUE
#' @export
#'
ch_sample <- function(con, std = TRUE, trim = TRUE) {
  
  q <- 
    tbl_mar(con, "channel.syni_v") |> 
    dplyr::select(stod_id,
                  synis_id,
                  veidarfaeri,
                  synaflokkur_nr,
                  tog_nr,
                  togbyrjun,
                  togendir,
                  tog_nr,
                  toghradi,
                  toglengd,
                  dplyr::everything())
  
  if(std) {
    q <- 
      q |> 
      # go metric
      dplyr::mutate(toglengd = 1852 * toglengd) |> 
      dplyr::rename(dplyr::any_of(vocabulary))
      
    if(trim) {
      q <- q %>% dplyr::select(.stid:towlength)
    }
  }
  
  return(q)
  
}

#' Sample class
#'
#' Reads channel.sample_category
#' 
#' @param con oracle connection
#' @param std create standardized/shortcut names (default is TRUE)
#' @param trim trim return only key variables (default is TRUE). only operational
#' if std is TRUE
#'
#' @return a tibble query
#' @export
#'
ch_sample_class <- function(con, std = TRUE, trim = TRUE) {
  
  q <- 
    tbl_mar(con, "channel.sample_category") |> 
    dplyr::select(sample_category_no,
                  eng_descr,
                  descr,
                  dplyr::everything())
  
  if(std) {
    q <-
      q |> 
      dplyr::rename(dplyr::any_of(vocabulary))
      
    if(trim) {
      q <-
        q |> 
        dplyr::select(sclass:synaflokkur)
    }
  }
  
  return(q)
}