#' Add colnames
#'
colnames_force <- function(df, col_names) {
  res <- data.frame(matrix(ncol = length(col_names), nrow = dim(df)[1]))
  colnames(res) <- col_names
  for(i in 1:dim(df)[2]) { res[,i] <- df[,i] }
  return(res)
}

#' Custom st_read
#'
#' @export
get_layer <- function(dsn, layer, crs) {
  #crs <- st_crs('crs')
  ll <- sf::st_read(dsn, layer)
  ll <- sf::st_zm(ll)
  ll <- sf::st_transform(ll, crs= crs)
  return(ll)
}

#' Force MULTILINESTRING to LINESTRING
#'
#' @export
multi2linestring <- function(layer) {
  gtype <- table(st_geometry_type(layer))
  if(gtype["MULTILINESTRING"] != 0) {
    for( i in 1 : dim(layer)[1] ) {
      df <- layer[i, ]
      gg <- st_geometry(df)
      ls <- st_cast(gg, "LINESTRING")
      if( length(ls) > 2 ) {
        stop("Can handle MULTILINESTRING with length =< 2")
      } else {
        if( length(ls) > 1 ) {
          sm <- st_line_sample(ls, sample = c(0, 1))
          sc <- st_coordinates(sm)
          ##
          coords <- st_coordinates(df)
          xy <- coords[, 1:2]
          l1 <- coords[, "L1"]
          ## Check if the end of first line intersect the start of the second line
          if( sum(abs(sc[2, 1:2] - sc[3, 1:2])) < 0.1 ) {
            new_ls <- st_geometry(st_linestring(xy))
          } else {
            ## If not, invert order:
            cc <- rbind(xy[l1 == 2, ], xy[l1 == 1, ])
            new_ls <- st_geometry(st_linestring(cc))
          }
          ls <- new_ls
        }
      }
      st_geometry(layer[i, ]) <- ls
    }
  }
  return(layer)
}

#' sample split point
#'
#' @export
sample_split_point <- function(point_near, link_near, min_dist) {
  ## Find a split point
  link_sample <- st_line_sample(link_near, n = st_length(link_near))
  link_points <- st_cast(link_sample, "POINT")
  dd <- as.numeric(st_distance(point_near, link_points))
  mm <- which(dd > min_dist)
  split_i <- mm[which.min(dd[mm])]
  point_split <- link_points[split_i]
  return(point_split)
}

#' sample split link
#'
#' @export
sample_split_link <- function(point_split, link_near) {
  link_snap <- st_snap(link_near, point_split, tol=1e-9)
  link_split <- st_split(link_snap, point_split)
  link_split <- st_cast(st_geometry(link_split), do_split=TRUE)
  return(link_split)
}

#' Link Nodes
#'
#' @export
link_nodes <-  function(layer, nodes) {
  start_nd <- st_line_sample(layer, sample = 0)
  end_nd <- st_line_sample(layer, sample = 1)
  ##
  start_d <- st_distance(start_nd, nodes)
  end_d <- st_distance(end_nd, nodes)
  start_i <- apply(start_d, 1, which.min)
  end_i <- apply(end_d, 1, which.min)
  ##
  data.frame(start_i, end_i)
}
