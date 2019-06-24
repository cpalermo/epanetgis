#' Add colnames
#'
colnames_force <- function(df, col_names) {
  res <- data.frame(matrix(ncol = length(col_names), nrow = dim(df)[1]))
  colnames(res) <- col_names
  for(i in 1:dim(df)[2]) { res[,i] <- df[,i] }
  return(res)
}

#' Force MULTILINESTRING in LINESTRING
#'
#' @export
multi2linestring <- function(layer) {
  layer <- st_zm(layer)
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
  return(layer)
}
