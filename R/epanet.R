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

#' Return coordinates from EPANET nodes
#'
#' @export
nd_coordinates <- function(dsn, node_model) {
  layer_list <- st_layers(dsn)$name
  layer_name <- layer_list[grep(nd$layer, layer_list, ignore.case = TRUE)]

  layer <- st_read(dsn, layer_name, quiet = TRUE, stringsAsFactors = FALSE)

  if (all(st_geometry_type(layer) == "POINT")) {
    format <- nd$format
    coordinates <- nd$coordinates

    ID <- eval(parse(text = format$ID))
    coord_X <- eval(parse(text = coordinates$coord_X))
    coord_Y <- eval(parse(text = coordinates$coord_Y))

    ## epanetReader
    Node <- as.character(ID)
    X.coord <- as.numeric(coord_X)
    Y.coord <- as.numeric(coord_Y)

    res <- data.frame(Node, X.coord, Y.coord, stringsAsFactors = FALSE)
    return(res)
  } else {
    stop("All geometries should be of type POINT")
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
    if(all(is.na(df[, i]))) {
      df[, i] <- as.logical(df[, i])
    } else {
      if(any(is.na(df[, i]))) {
        df[, i] <- as.factor(df[, i])
      }
    }
  }

  return(df)

}
