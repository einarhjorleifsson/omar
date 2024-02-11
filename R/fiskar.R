# Old legacies

#' Title
#'
#' The current form is just mimicry
#'
#' @param con XX
#' @param schema XX
#'

lesa_stodvar <- function(con, schema = "fiskar") {
  
  tbl_mar(con, paste0(schema, ".stodvar")) %>%
    dplyr::select(synis_id:heildarafli, synaflokkur, net_nr) %>%
    dplyr::mutate(ar = to_number(to_char(dags, "YYYY"))) %>%
    dplyr::left_join(tbl_mar(con, paste0(schema, ".togstodvar")) %>%
                       dplyr::select(synis_id:eykt), by = "synis_id") %>%
    dplyr::left_join(tbl_mar(con, paste0(schema, ".umhverfi")) %>%
                       dplyr::select(synis_id:sjondypi), by = "synis_id") %>%
    dplyr::mutate(synis_id = dplyr::if_else(schema == "fiskar", synis_id, -synis_id),
                  index = reitur * 100 + tognumer)
  
}


#' Title
#'
#' Bla, bla, ...
#'
#' @param con XXX
#' @param schema XXX
#'
#'
lesa_lengdir <- function(con, schema = "fiskar") {
  
  tbl_mar(con, paste0(schema, ".lengdir")) %>%
    dplyr::select(synis_id:kynthroski) %>%
    dplyr::mutate(synis_id = dplyr::if_else(schema == "fiskar", synis_id, -synis_id))
  
}

#' Title
#'
#' Some text
#'
#' @param con XXX
#' @param schema XXX
#'
lesa_kvarnir <- function(con, schema = "fiskar") {
  
  tbl_mar(con, paste0(schema, ".kvarnir")) %>%
    dplyr::select(synis_id:kynfaeri, lifur, magi, syking = sy) %>%
    dplyr::mutate(synis_id = dplyr::if_else(schema == "fiskar", synis_id, -synis_id))
  
}

#' Title
#'
#' Bla, bla,
#'
#' @param con XXX
#' @param schema XXX
#'
#'
lesa_numer <- function(con, schema = "fiskar") {
  
  tbl_mar(con, paste0(schema, ".numer")) %>%
    dplyr::select(synis_id, tegund, fj_maelt, fj_talid, fj_kyngreint,
                  fj_vigtad, fj_magasyna) %>%
    dplyr::mutate(synis_id = dplyr::if_else(schema == "fiskar", synis_id, -synis_id),
                  fj_maelt = dplyr::if_else(is.na(fj_maelt), 0, fj_maelt),
                  fj_talid = dplyr::if_else(is.na(fj_talid), 0, fj_talid),
                  fj_kyngreint = dplyr::if_else(is.na(fj_kyngreint), 0, fj_kyngreint),
                  fj_vigtad = dplyr::if_else(is.na(fj_vigtad), 0, fj_vigtad),
                  fj_magasyna = dplyr::if_else(is.na(fj_magasyna), 0, fj_vigtad),
                  fj_alls = fj_maelt + fj_talid)
  
}
