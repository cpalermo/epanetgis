#' Add colnames
#'
colnames_force <- function(df, col_names) {
  res <- data.frame(matrix(ncol = length(col_names), nrow = dim(df)[1]))
  colnames(res) <- col_names
  for(i in 1:dim(df)[2]) { res[,i] <- df[,i] }
  return(res)
}
