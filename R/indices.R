# TODO: 
#  think about dividing strata area to each station, this would be
#     year and sclass specific

#' smx_st
#'
#' Returns a station table by a join of channel.stod_v and channel.syni_v, 
#' indented for standariszed survey algorithm.
#' 
#' @param con oracle connection
#' @param sclass A numerical vector indicating sampling class (synaflokkur)
#' @param stratification Any of "old_strata", "new_strata", "ghl_strata"
#' @param trim return only "key" variables (default TRUE)
#' @param trim_towlength A boolean (default TRUE) that trims towlength
#'
#' @return a tibble query
#' @export
#'
smx_st <- function(con, sclass = c(19, 30, 31, 34, 35, 37, 40), stratification = "new_strata",
                   trim = TRUE,
                   trim_towlength = TRUE) {
  
  q <- 
    ch_station(con) %>% 
    dplyr::left_join(ch_sample(con),
                     by = ".stid") %>% 
    dplyr::filter(sclass %in% local(sclass)) %>% 
    dplyr::mutate(idx = sq * 10000 + nvl(tid, 0) * 100 + gid) %>% 
    dplyr::left_join(tbl_mar(con, 'biota.strata_stations') %>% 
                       dplyr::rename(idx = station,
                                     sclass = synaflokkur) %>% 
                       dplyr::filter(stratification %in% local(stratification)),
                     by = c("sclass", "idx"))
  
  if(trim_towlength) {
  q <- 
    q %>% 
    # idea plagiarize from tidypax
    #  Note: ops$einarhj.tow_metrics not public
    #        check what source tidypax uses, if personal bjarki - should be made "global"
    #        not the table contains default acronyms for surveys, i.d. smb, smh, smn, smr, smi, smg, ..
    dplyr::left_join(tbl_mar(con, "ops$einarhj.tow_metrics"),
                     by = "sclass") %>% 
    dplyr::mutate(towlength = dplyr::case_when(towlength == 0 ~ 1,
                                                towlength > nvl(max_towlength, 1e6) ~ nvl(max_towlength, 1),
                                                towlength < nvl(min_towlength, 0) ~ nvl(min_towlength, 1),
                                                TRUE ~ nvl(towlength, 1))) %>% 
    # Need to specify units for area swept
    dplyr::mutate(area_swept  = towlength *  nvl(std_width, 1) * ifelse(gid == 78, 1.25, 1)) %>% 
    dplyr::select(-c(min_towlength, max_towlength, std_towlength, std_width))
  # here we could calculate an rt-factor that downstream is used to raise n
  
  }
  
  if(trim) {
    q <- 
      q %>% 
      dplyr::select(year, tid, sq, lon, lat, lon2, lat2, st, bt, .id, gid, sclass, sur, idx:stratum, area_swept)
  }
  return(q)
  
}



#' smx_length
#'
#' Returns a 
#' 
#' @param con oracle connection
#' @param sid species numerical code
#' @param sclass A numerical vector indicating sampling class (synaflokkur)
#' @param stratification Any or all of "old_strata", "new_strata", "ghl_strata"
#'
#' @return a tibble query
#' @export
#'
smx_len <- function(con, 
                    sid = 1, 
                    sclass = c(19, 30, 31, 34, 35, 37, 40),
                    stratification = "new_strata") {
  
  q_sid_dummies <- 
    tidyr::expand_grid(dummy = 1L,
                       sid = sid) %>% 
    tibble_to_dual(con) %>% 
    suppressMessages()
  
  smx_st(con, sclass = sclass, stratification = stratification) %>% 
    dplyr::mutate(dummy = 1L) %>% 
    dplyr::left_join(q_sid_dummies,
                     by = "dummy") %>% 
    dplyr::select(-dummy) %>% 
    dplyr::left_join(bi_len(con),
                     by = c(".id", "sid"))
  
}


smx_age <- function() {
  
}

smx_alk <- function() {
  
}


tibble_to_dual <- 
  function (dat, mar) {
    names(dat) %>% 
      purrr::set_names(., .) %>% 
      purrr::map(~sprintf("%s as \"%s\"", ifelse(dat[[.]] %>% purrr::map(is.numeric) %>% unlist(),
                                                 dat[[.]], paste0("'", dat[[.]], "'")), .)) %>% 
      purrr::map(tibble::as_tibble) %>% 
      dplyr::bind_cols() %>% tidyr::unite("sql", sep = ", ") %>% 
      dplyr::mutate(sql = paste("select ", sql, "from dual")) %>% 
      .$sql %>% paste(collapse = " union all ") %>% dplyr::sql() %>% 
      dplyr::tbl(mar, .)
  }