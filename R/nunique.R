#' nunique
#'
#' Get number of unique values in a vector or column of a dataframe
#' @param x vector or column of a dataframe
#' @return atomic number listing number of unique values in x
#' @examples
#' vec = c(1,3,4,2,5,5,5,1,1,3,7,9)
#' nunique(vec)

nunique = function(x){
  return(length(unique(x)))
}
