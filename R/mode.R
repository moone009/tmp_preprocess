#' @title Mode
#' @description
#' Computes the mode of a vector, list, or data frame column vector
#' 
#' @param x is your inpute value
#' @return A single value containing the mode of input x
#' @export
#' 

mode <- function(x){
  
  idx <- table(t(x))
  max <- names(idx[which(idx %in% max(idx))])
  if(length(idx)==1){
    return(names(idx))
  }else if(length(max) == 1){
    return(max)
  }else{
    return('tie')
  }
  
}

