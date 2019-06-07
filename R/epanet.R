#' Create a new .inp object
#'
#' @description
#' Returns a new epanet.inp S3 object
#'
#' @usage
#' en_new()
#'
#' @examples
#' net <- en_new()
#' class(net)
#' # [1] "epanet.inp"
#'
#' @export
en_new <- function() {
  conn <- system.file("extdata", "new.inp", package = "epanetgis")
  epanetReader::read.inp(conn)
}
