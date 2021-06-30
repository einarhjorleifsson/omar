#' Connection to mar
#'
#'
#' @param con oracle connection
#' @param tbl a string specifying schema.table
#'
#' @return q query
#' @export
#'

tbl_mar <- function(con, tbl) {
  x <- strsplit(tbl,'\\.') %>% unlist() %>%
      purrr::map_if(!grepl('\"',.), toupper) %>%
      gsub('\"','',.)
  dplyr::tbl(con, dbplyr::in_schema(x[1], x[2])) %>%
    dplyr::select_all(tolower)
}


#' Connection
#'
#' @param dbname database name (default is sjor)
#' @param ... additional argument to pass to dbConnect
#'
#' @return a connection
#' @export
#'
connect_mar <- function (dbname = "sjor", ...) 
{
  ROracle::dbConnect(DBI::dbDriver("Oracle"), dbname = dbname, 
                     ...)
}
