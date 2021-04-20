#' Connect to MFRI database
#'
#' @param ... Additional arguments to DBI functions
#'
#' @return A connection to the Oracle database
#' @export
#'

con_mar <- function(...) {
  DBI::dbConnect(DBI::dbDriver("Oracle"), ...)
}

#' Connection to MAR tables
#'
#' @param con The connection
#' @param tbl The name of the table (normally schema.table)
#'
#' @return a query
#' @export
#'
tbl_mar <- function(con, tbl) {
  x <- strsplit(tbl,'\\.') %>% unlist()
  dplyr::tbl(con, dbplyr::in_schema(x[1], x[2])) %>%
    dplyr::select_all(tolower)
}