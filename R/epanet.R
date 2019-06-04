#' Check EPANET session
#'
#' @examples
#' en_open()
#'
#' @export
en_open <- function() {
  res <- epanet2toolkit::getOpenflag()
  return(res)
}
