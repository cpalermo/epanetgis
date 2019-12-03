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

#' Return coordinates from EPANET node
#'
#' @export
nd_get_coordinates <- function(dsn, node_model) {

  if (!is.null(node_model$layer)) {
    layer_list <- st_layers(dsn)$name
    layer_name <- node_model$layer
    #layer_name <- layer_list[grep(node_model$layer, layer_list, ignore.case = TRUE)]

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
  } else {
    res <- data.frame()
  }
}

#' Return vertices from EPANET link
#'
#' @export
ln_get_vertices <- function(dsn, link_model, model) {

  if (!is.null(link_model$layer)) {
    layer_list <- st_layers(dsn)$name
    layer_name <- link_model$layer
    #layer_name <- layer_list[grep(link_model$layer, layer_list, ignore.case = TRUE)]

    layer <- st_read(dsn, layer_name, quiet = TRUE, stringsAsFactors = FALSE)
    nodes <- en_coordinates(dsn, model)
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

#' Return coordinates from EPANET nodes
#'
#' @export
nd_coordinates <- function(dsn, node, model) {
  nd_list <- model[[node]]
  df <- data.frame()
  for(i in 1:length(nd_list)) {
    nd <- nd_list[[i]]
    df <- rbind(df, nd_get_coordinates(dsn, nd))
  }
  return(df)
}

#' Return vertices from EPANET links
#'
#' @export
ln_vertices <- function(dsn, link, model) {
  ln_list <- model[[link]]
  df <- data.frame()
  for(i in 1:length(ln_list)) {
    ln <- ln_list[[i]]
    df <- rbind(df, ln_get_vertices(dsn, ln, model))
  }
  return(df)
}

#' Return EPANET coordinates from all nodes
#'
#' @export
en_coordinates <- function(dsn, model) {
  jun <- nd_coordinates(dsn, "junctions", model)
  res <- nd_coordinates(dsn, "reservoirs", model)
  tan <- nd_coordinates(dsn, "tanks", model)

  df <- rbind(jun, res, tan)

  return(df)
}

#' Return EPANET vertices from all links
#'
#' @export
en_vertices <- function(dsn, model) {
  pip <- ln_vertices(dsn, "pipes", model)
  pum <- ln_vertices(dsn, "pumps", model)
  val <- ln_vertices(dsn, "valves", model)

  df <- rbind(pip, pum, val)

  return(df)
}

#' Return EPANET [JUNCTIONS]
#'
#' @export
nd_get_junctions <- function(dsn, node_model) {
  if (!is.null(node_model$layer)) {
    layer_list <- st_layers(dsn)$name
    layer_name <- node_model$layer
    #layer_name <- layer_list[grep(node_model$layer, layer_list, ignore.case = TRUE)]

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
      elevation <- eval(parse(text = format$elevation))
      demand_flow <- eval(parse(text = format$demand_flow))

      ## epanetReader
      ID <- as.character(ID)
      Elevation <- as.numeric(elevation)
      Demand <- as.numeric(demand_flow)

      res <- data.frame(ID,
                        Elevation,
                        Demand,
                        stringsAsFactors = FALSE)
      res[is.na(res)] <- 0

      return(res)
    } else {
      stop("All geometries should be of type POINT")
    }
  } else {
    res <- data.frame()
  }
}

#' Return EPANET [RESERVOIRS]
#'
#' @export
nd_get_reservoirs <- function(dsn, node_model) {
  if (!is.null(node_model$layer)) {
    layer_list <- st_layers(dsn)$name
    layer_name <- node_model$layer
    #layer_name <- layer_list[grep(node_model$layer, layer_list, ignore.case = TRUE)]

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
      head <- eval(parse(text = format$head))

      ## epanetReader
      ID <- as.character(ID)
      Head <- as.numeric(head)

      res <- data.frame(ID,
                        Head,
                        stringsAsFactors = FALSE)
      res[is.na(res)] <- 0

      return(res)
    } else {
      stop("All geometries should be of type POINT")
    }
  } else {
    res <- data.frame()
  }
}

#' Return EPANET [TANKS]
#'
#' @export
nd_get_tanks <- function(dsn, node_model) {
  if (!is.null(node_model$layer)) {
    layer_list <- st_layers(dsn)$name
    layer_name <- node_model$layer
    #layer_name <- layer_list[grep(node_model$layer, layer_list, ignore.case = TRUE)]

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
      elevation <- eval(parse(text = format$elevation))
      initial_level <- eval(parse(text = format$initial_level))
      minimum_level <- eval(parse(text = format$minimum_level))
      maximum_level <- eval(parse(text = format$maximum_level))
      diameter <- eval(parse(text = format$diameter))
      minimum_volume <- eval(parse(text = format$minimum_volume))

      ## epanetReader
      ID <- as.character(ID)
      Elevation <- elevation
      InitLevel <- initial_level
      MinLevel <- minimum_level
      MaxLevel <- maximum_level
      Diameter <- diameter
      MinVol <- minimum_volume

      res <- data.frame(ID,
                        Elevation,
                        InitLevel,
                        MinLevel,
                        MaxLevel,
                        Diameter,
                        MinVol,
                        stringsAsFactors = FALSE)
      res[is.na(res)] <- 0

      return(res)
    } else {
      stop("All geometries should be of type POINT")
    }
  } else {
    res <- data.frame()
  }
}

#' Return EPANET nodes
#'
#' @export
nd_nodes <- function(dsn, node, model) {
  nd_list <- model[[node]]
  df <- data.frame()
  for(i in 1:length(nd_list)) {
    nd <- nd_list[[i]]
    if(node == "junctions") {df <- rbind(df, nd_get_junctions(dsn, nd))}
    if(node == "reservoirs") {df <- rbind(df, nd_get_reservoirs(dsn, nd))}
    if(node == "tanks") {df <- rbind(df, nd_get_tanks(dsn, nd))}
  }
  return(df)
}

####


#' Return EPANET [PIPES]
#'
#' @export
ln_get_pipes <- function(dsn, link_model, model) {
  if (!is.null(link_model$layer)) {
    layer_list <- st_layers(dsn)$name
    layer_name <- link_model$layer
    #layer_name <- layer_list[grep(link_model$layer, layer_list, ignore.case = TRUE)]

    layer <- st_read(dsn, layer_name, quiet = TRUE, stringsAsFactors = FALSE)
    nodes <- en_coordinates(dsn, model)
  } else {
    layer <- data.frame()
  }

  if (dim(layer)[1] > 0) {
    layer <- multi2linestring(layer)

    if (all(st_geometry_type(layer) == "LINESTRING")) {
      eval(parse(text = link_model$variables))
      vertices <- link_model$vertices
      format <- link_model$format

      ## [PIPES] specific
      ID <- eval(parse(text = format$ID))
      start_node_ID <- eval(parse(text = format$start_node_ID))
      end_node_ID <- eval(parse(text = format$end_node_ID))
      length <- eval(parse(text = format$length))
      diameter <- eval(parse(text = format$diameter))
      roughness <- eval(parse(text = format$roughness))
      ## epanetReader
      ID <- as.character(ID)
      Node1 <- as.character(start_node_ID)
      Node2 <- as.character(end_node_ID)
      Length <- as.numeric(length)
      Diameter <- as.numeric(diameter)
      Roughness <- as.numeric(roughness)
      res <- data.frame(ID,
                        Node1,
                        Node2,
                        Length,
                        Diameter,
                        Roughness,
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

#' Return EPANET [PUMPS]
#'
#' @export
ln_get_pumps <- function(dsn, link_model, model) {
  if (!is.null(link_model$layer)) {
    layer_list <- st_layers(dsn)$name
    layer_name <- link_model$layer
    #layer_name <- layer_list[grep(link_model$layer, layer_list, ignore.case = TRUE)]

    layer <- st_read(dsn, layer_name, quiet = TRUE, stringsAsFactors = FALSE)
    nodes <- en_coordinates(dsn, model)
  } else {
    layer <- data.frame()
  }

  if (dim(layer)[1] > 0) {
    layer <- multi2linestring(layer)

    if (all(st_geometry_type(layer) == "LINESTRING")) {
      eval(parse(text = link_model$variables))
      vertices <- link_model$vertices
      format <- link_model$format

      ## [PUMPS] specific
      ID <- eval(parse(text = format$ID))
      start_node_ID <- eval(parse(text = format$start_node_ID))
      end_node_ID <- eval(parse(text = format$end_node_ID))
      ## properties
      ##
      ## epanetReader
      ID <- as.character(ID)
      Node1 <- as.character(start_node_ID)
      Node2 <- as.character(end_node_ID)
      res <- data.frame(ID,
                        Node1,
                        Node2,
                        stringsAsFactors = FALSE)
      ## end [PUMPS] specific

      return(res)
    } else {
      stop("All geometries should be of type LINESTRING")
    }
  } else {
    res <- data.frame()
    return(res)
  }
}

#' Return EPANET [VALVES]
#'
#' @export
ln_get_valves <- function(dsn, link_model, model) {
  if (!is.null(link_model$layer)) {
    layer_list <- st_layers(dsn)$name
    layer_name <- link_model$layer
    #layer_name <- layer_list[grep(link_model$layer, layer_list, ignore.case = TRUE)]

    layer <- st_read(dsn, layer_name, quiet = TRUE, stringsAsFactors = FALSE)
    nodes <- en_coordinates(dsn, model)
  } else {
    layer <- data.frame()
  }

  if (dim(layer)[1] > 0) {
    layer <- multi2linestring(layer)

    if (all(st_geometry_type(layer) == "LINESTRING")) {
      eval(parse(text = link_model$variables))
      vertices <- link_model$vertices
      format <- link_model$format

      ## [VALVES] specific
      ID <- eval(parse(text = format$ID))
      start_node_ID <- eval(parse(text = format$start_node_ID))
      end_node_ID <- eval(parse(text = format$end_node_ID))
      ## diameter
      ## type
      ## setting
      ## minor_loss
      ##
      ## epanetReader
      ID <- as.character(ID)
      Node1 <- as.character(start_node_ID)
      Node2 <- as.character(end_node_ID)
      res <- data.frame(ID,
                        Node1,
                        Node2,
                        stringsAsFactors = FALSE)
      ## end [VALVES] specific

      return(res)
    } else {
      stop("All geometries should be of type LINESTRING")
    }
  } else {
    res <- data.frame()
    return(res)
  }
}

#' Return EPANET links
#'
#' @export
ln_links <- function(dsn, link, model) {
  ln_list <- model[[link]]
  df <- data.frame()
  for(i in 1:length(ln_list)) {
    ln <- ln_list[[i]]
    if(link == "pipes") {df <- rbind(df, ln_get_pipes(dsn, ln, model))}
    if(link == "pumps") {df <- rbind(df, ln_get_pumps(dsn, ln, model))}
    if(link == "valves") {df <- rbind(df, ln_get_valves(dsn, ln, model))}
  }
  return(df)
}

#' Return EPANET node data
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
