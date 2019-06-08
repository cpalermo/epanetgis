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

#' Return coordinates form EPANET nodes
#'
#' @export
nd_coordinates <- function(nodes) {
  if (all(st_geometry_type(x = nodes) == "POINT")) {
    coordinates <- data.frame(nodes$ID, st_coordinates(nodes), stringsAsFactors = FALSE)
    names(coordinates) <- c("Node", "X.coord", "Y.coord")
    return(coordinates)
  } else {
    stop("All geometry should be of type POINT")
  }
}

#' Return an EPANET node
#'
#' @export
nd_data <- function(nodes, type) {
  jun_names <- c("ID",
                 "Elevation",
                 "Demand",
                 "Pattern")
  res_names <- c("ID",
                 "Head",
                 "Pattern")
  tan_names <- c("ID",
                 "Elevation",
                 "InitLevel",
                 "MinLevel",
                 "MaxLevel",
                 "Diameter",
                 "MinVol",
                 "VolCurve")

  df <- st_drop_geometry(nodes)

  if (type == "junctions") {df <- colnames_force(df, jun_names)}
  if (type == "reservoirs") {df <- colnames_force(df, res_names)}
  if (type == "tanks") {df <- colnames_force(df, tan_names)}

  for(i in 1:dim(df)[2]) {
    if(all(is.na(df[, i]))) {df[, i] <- as.logical(df[, i])}
  }

  return(df)
}
