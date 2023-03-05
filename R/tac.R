#' TAC
#'
#' @param con oracle connection
#'
#' @return a queary
#' @export
tac <- function(con) {
  tbl_mar(con, "ops$einarhj.tac")
}