#' Check if IMO is valid
#' 
#' https://en.wikipedia.org/wiki/IMO_number
#'
#' @param x a numerical vector
#'
#' @return A boolean TRUE/FALSE vector
#' @export
#'
valid_imo <- function(x) {
  lh <- function(x, n) { str_sub(x, n, n) %>% as.integer() }
  #if(nchar(x) != 7) return(FALSE)
  x2 <- 
    lh(x, 1) * 7 +
    lh(x, 2) * 6 +
    lh(x, 3) * 5 +
    lh(x, 4) * 4 +
    lh(x, 5) * 3 +
    lh(x, 6) * 2
  return(str_sub(x2, nchar(x2)) == str_sub(x, nchar(x)))
}


