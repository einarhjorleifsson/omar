#' Connection to mar
#'
#' @name tbl_mar
#'
#' @param con tenging við oraclegrunn
#' @param tbl nafn oracle töflu
#'
#' @return sql query and data
#' @export
#'

tbl_mar <- function(con, tbl) {
  x <- strsplit(tbl,'\\.') %>% unlist() %>%
      purrr::map_if(!grepl('\"',.), toupper) %>%
      gsub('\"','',.)
  dplyr::tbl(con, dbplyr::in_schema(x[1], x[2])) %>%
    dplyr::select_all(tolower)
}


#' Title
#'
#' @param dbname 
#' @param ... 
#'
#' @return
#' @export
#'
connect_mar <- function (dbname = "sjor", ...) 
{
  ROracle::dbConnect(DBI::dbDriver("Oracle"), dbname = dbname, 
                     ...)
}
