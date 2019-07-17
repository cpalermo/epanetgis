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
sw_new <- function() {
  conn <- system.file("extdata", "new.inp", package = "epanetgis")
  epanetReader::read.inp(conn)
}

#' Return SWMM coordinates from all nodes
#'
#' @export
sw_coordinates <- function(dsn, model) {
  jun <- nd_coordinates(dsn, "junctions", model)
  out <- nd_coordinates(dsn, "outfalls", model)
  df <- rbind(jun, out)
  return(df)
}

#' Return SWMM vertices from all links
#'
#' @export
sw_vertices <- function(dsn, model) {
  con <- sw_ln_vertices(dsn, "conduits", model)
  ##    ¯¯¯¯
  df <- rbind(con)

  return(df)
}

#' Return vertices from SWMM links
#'
#' @export
sw_ln_vertices <- function(dsn, link, model) {
  ln_list <- model[[link]]
  df <- data.frame()
  for(i in 1:length(ln_list)) {
    ln <- ln_list[[i]]
    df <- rbind(df, sw_get_vertices(dsn, ln, model))
    ##             ¯¯¯¯
  }
  return(df)
}

#' Return vertices from SWMM link
#'
#' @export
sw_get_vertices <- function(dsn, link_model, model) {

  if (!is.null(link_model$layer)) {
    layer_list <- st_layers(dsn)$name
    layer_name <- layer_list[grep(link_model$layer, layer_list, ignore.case = TRUE)]

    layer <- st_read(dsn, layer_name, quiet = TRUE, stringsAsFactors = FALSE)
    nodes <- sw_coordinates(dsn, model)
    ##      ¯¯¯¯
  } else {
    layer <- data.frame()
  }

  if (dim(layer)[1] > 0) {
    layer <- multi2linestring(layer)

    if (all(st_geometry_type(layer) == "LINESTRING")) {
      eval(parse(text = link_model$variables))
      vertices <- link_model$vertices
      format <- link_model$format

      line_i <- eval(parse(text = vertices$line_i))
      ID <- eval(parse(text = format$ID))
      ID <- ID[line_i]
      coord_X <- eval(parse(text = vertices$coord_X))
      coord_Y <- eval(parse(text = vertices$coord_Y))

      ## epanetReader
      ID <- as.character(ID)
      X.coord <- as.numeric(coord_X)
      Y.coord <- as.numeric(coord_Y)

      res <- data.frame(ID, X.coord, Y.coord, stringsAsFactors = FALSE)
      return(res)
    } else {
      stop("All geometries should be of type LINESTRING")
    }
  } else {
    res <- data.frame()
    return(res)
  }
}

#' Return SWMM [JUNCTIONS]
#'
#' @export
sw_get_junctions <- function(dsn, node_model) {
  if (!is.null(node_model$layer)) {
    layer_list <- st_layers(dsn)$name
    layer_name <- layer_list[grep(node_model$layer, layer_list, ignore.case = TRUE)]

    layer <- st_read(dsn, layer_name, quiet = TRUE, stringsAsFactors = FALSE)
  } else {
    layer <- data.frame()
  }

  if (dim(layer)[1] > 0) {
    if (all(st_geometry_type(layer) == "POINT")) {
      eval(parse(text = node_model$variables))
      format <- node_model$format
      coordinates <- node_model$coordinates

      ID <- eval(parse(text = format$ID))
      invert_elevation <- eval(parse(text = format$invert_elevation))
      max_depth <- eval(parse(text = format$max_depth))
      init_depth <- eval(parse(text = format$init_depth))
      surcharge_depth <- eval(parse(text = format$surcharge_depth))
      ponded_area <- eval(parse(text = format$ponded_area))

      ## epanetReader
      ID <- as.character(ID)
      Elevation <- as.numeric(invert_elevation)
      Demand <- as.numeric(max_depth)
      Pattern <- as.numeric(init_depth)
      Surcharge <- as.numeric(surcharge_depth)
      Area <- as.numeric(ponded_area)

      res <- data.frame(ID,
                        Elevation,
                        Demand,
                        Pattern,
                        Surcharge,
                        Area,
                        stringsAsFactors = FALSE)

      ## TODO:
      res[is.na(res)] <- 0

      return(res)
    } else {
      stop("All geometries should be of type POINT")
    }
  } else {
    res <- data.frame()
  }
}

#' Return SWMM [OUTFALLS]
#'
#' @export
sw_get_outfalls <- function(dsn, node_model) {
  if (!is.null(node_model$layer)) {
    layer_list <- st_layers(dsn)$name
    layer_name <- layer_list[grep(node_model$layer, layer_list, ignore.case = TRUE)]

    layer <- st_read(dsn, layer_name, quiet = TRUE, stringsAsFactors = FALSE)
  } else {
    layer <- data.frame()
  }

  if (dim(layer)[1] > 0) {
    if (all(st_geometry_type(layer) == "POINT")) {
      eval(parse(text = node_model$variables))
      format <- node_model$format
      coordinates <- node_model$coordinates

      ID <- eval(parse(text = format$ID))
      invert_elevation <- eval(parse(text = format$invert_elevation))

      ## epanetReader
      ID <- as.character(ID)
      Elevation <- as.numeric(invert_elevation)

      res <- data.frame(ID,
                        Elevation,
                        stringsAsFactors = FALSE)

      ## TODO:
      res[is.na(res)] <- 0

      return(res)
    } else {
      stop("All geometries should be of type POINT")
    }
  } else {
    res <- data.frame()
  }
}

#' Return SWMM nodes
#'
#' @export
sw_nodes <- function(dsn, node, model) {
  nd_list <- model[[node]]
  df <- data.frame()
  for(i in 1:length(nd_list)) {
    nd <- nd_list[[i]]
    if(node == "junctions") {df <- rbind(df, sw_get_junctions(dsn, nd))}
    if(node == "outfalls") {df <- rbind(df, sw_get_outfalls(dsn, nd))}
  }
  return(df)
}



#' Return SWMM [CONDUITS]
#'
#' @export
sw_get_conduits <- function(dsn, link_model, model) {
  if (!is.null(link_model$layer)) {
    layer_list <- st_layers(dsn)$name
    layer_name <- layer_list[grep(link_model$layer, layer_list, ignore.case = TRUE)]

    layer <- st_read(dsn, layer_name, quiet = TRUE, stringsAsFactors = FALSE)
    nodes <- sw_coordinates(dsn, model)
    ##      ¯¯¯
  } else {
    layer <- data.frame()
  }

  if (dim(layer)[1] > 0) {
    layer <- multi2linestring(layer)

    if (all(st_geometry_type(layer) == "LINESTRING")) {
      eval(parse(text = link_model$variables))
      vertices <- link_model$vertices
      format <- link_model$format

      ## [CONDUITS] specific
      ID <- eval(parse(text = format$ID))
      start_node_ID <- eval(parse(text = format$start_node_ID))
      end_node_ID <- eval(parse(text = format$end_node_ID))
      length <- eval(parse(text = format$length))
      manning <- eval(parse(text = format$manning))
      inlet_offset <- eval(parse(text = format$inlet_offset))
      outlet_offset <- eval(parse(text = format$outlet_offset))
      ## epanetReader
      ID <- as.character(ID)
      Node1 <- as.character(start_node_ID)
      Node2 <- as.character(end_node_ID)
      Length <- as.numeric(length)
      Diameter <- as.numeric(manning) ## NB TODO
      InletOffset <- as.numeric(inlet_offset)
      OutletOffset <- as.numeric(outlet_offset)
      res <- data.frame(ID,
                        Node1,
                        Node2,
                        Length,
                        Diameter, ## NB TODO
                        InletOffset,
                        OutletOffset,
                        stringsAsFactors = FALSE)
      ## end [PIPES] specific

      return(res)
    } else {
      stop("All geometries should be of type LINESTRING")
    }
  } else {
    res <- data.frame()
    return(res)
  }
}

#' Return SWMM links
#'
#' @export
sw_links <- function(dsn, link, model) {
  ln_list <- model[[link]]
  df <- data.frame()
  for(i in 1:length(ln_list)) {
    ln <- ln_list[[i]]
    if(link == "conduits") {df <- rbind(df, sw_get_conduits(dsn, ln, model))}
  }
  return(df)
}
