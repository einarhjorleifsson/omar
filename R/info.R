#' mar_tables
#'
#' @description Fallið myndar tengingu við schema all_tables
#'
#' @name mar_tables
#'
#' @param mar src_oracle tenging við oracle
#' @param schema character vector specifying schema name, e.g. "fiskar"
#'
#' @return dataframe
#' @export
mar_tables <- function(mar, schema) {
  
  d <- tbl_mar(mar, "sys.all_tables")
  #  v <- tbl_mar(mar,'sys.all_views') %>%
  #    dplyr::select(owner,table_name = view_name) %>%
  #    dplyr::mutate(tablespace_name = NULL, num_rows = NULL, last_analyze = NULL)
  if(!missing(schema)) {
    d <- d %>% dplyr::filter(owner %in% toupper(schema))
  }
  
  d %>%
    dplyr::select(owner, table_name, tablespace_name, num_rows, last_analyzed) %>%
    dplyr::left_join(tbl_mar(mar,'sys.all_tab_comments')) %>%
    dplyr::mutate(owner = lower(owner),
                  table_name = lower(table_name)) %>%
    dplyr::select(owner, table_name, comments, tablespace_name, num_rows, last_analyzed)
  
}

#' mar_fields
#'
#' @description Fallið ...
#'
#' @name mar_fields
#'
#' @param mar src_oracle tenging við oracle
#' @param table character vector specifying table names, e.g. "channel.stod_v"
#'
#'
#' @return dataframe
#' @export
mar_fields <- function(mar, table) {
  
  x <- strsplit(table,'\\.') %>% unlist()
  
  #  tbl_mar(mar,'all_tab_columns') %>%
  #    dplyr::filter(owner == toupper(x[1]),
  #        t          table_name == toupper(x[2])) %>%
  #    dplyr::transmute(owner = lower(owner),
  #                     table_name = lower(table_name),
  #                     column_name = lower(column_name),
  #                     data_type = lower(data_type))
  
  tbl_mar(mar,'sys.all_col_comments') %>%
    dplyr::filter(owner == local(toupper(x[1])),
                  table_name == local(toupper(x[2]))) %>%
    dplyr::transmute(owner = lower(owner),
                     table_name = lower(table_name),
                     column_name = lower(column_name),
                     comments = comments)
}


#' mar_views
#'
#' @description Fallið myndar tengingu við sys.all_views
#'
#' @name mar_views
#'
#' @param con src_oracle tenging við oracle
#' @param schema character vector specifying schema name, e.g. "fiskar"
#'
#' @return dataframe
#' @export
mar_views <- function(con, schema) {
  
  d <-
    dplyr::tbl(con, sql("select OWNER, VIEW_NAME from sys.all_views")) %>%
    dplyr::rename(owner = OWNER,
                  view_name = VIEW_NAME)
  if (!missing(schema)) {
    d <- d %>% dplyr::filter(owner %in% toupper(schema))
  }
  d %>%
    dplyr::mutate(owner = tolower(owner),
                  view_name = tolower(view_name))
}
